#!/usr/bin/env stack
-- stack --resolver lts-8.9 script
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import ClassyPrelude.Conduit
import Network.HTTP.Simple
import Data.Yaml
import Data.Aeson (withObject)
import Control.Monad.State.Strict (StateT (..), execStateT, MonadState (..), modify)
import Text.HTML.DOM (sinkDoc)
import Text.XML.Cursor
import qualified Data.Text as T

data PurifyYaml = PurifyYaml
  { pyOutputFile :: !Text
  , pyExtraDeps :: !(HashMap Text ExtraDep)
  }
instance ToJSON PurifyYaml where
  toJSON (PurifyYaml out deps) = object
    [ "output-file" .= out
    , "extra-deps" .= deps
    ]
instance FromJSON PurifyYaml where
  parseJSON = withObject "PurifyYaml" $ \o -> PurifyYaml
    <$> o .: "output-file"
    <*> o .: "extra-deps"
data ExtraDep = ExtraDep
  { edRepo :: !Text
  , edCommit :: !Text
  , edDeps :: !(Set Text)
  }
instance ToJSON ExtraDep where
  toJSON (ExtraDep repo commit deps) = object
    [ "repo" .= repo
    , "commit" .= commit
    , "deps" .= deps
    ]
instance FromJSON ExtraDep where
  parseJSON = withObject "ExtraDep" $ \o -> ExtraDep
    <$> o .: "repo"
    <*> o .: "commit"
    <*> o .: "deps"

main :: IO ()
main = do
  packages <- getArgs
  PurifyYaml outputFile deps0 <- decodeFileEither "purify.yaml"
                             >>= either throwM return
  deps <- execStateT (mapM_ (addDep outputFile) packages) deps0
  encodeFile "purify.yaml" $ PurifyYaml outputFile deps

-- FIXME detect cycles?
addDep :: Text -> Text -> StateT (HashMap Text ExtraDep) IO ()
addDep outputFile name = do
  allDeps <- get
  case lookup name allDeps of
    Nothing -> do
      say $ "Adding dep: " ++ name
      (repo, deps) <- liftIO $ lookupPackage name
      master <- liftIO $ getMasterCommit repo
      modify $ insertMap name $ ExtraDep repo master deps
      deps' <- get
      liftIO $ encodeFile "purify.yaml" $ PurifyYaml outputFile deps'
      mapM_ (addDep outputFile) deps
    Just ed -> mapM_ (addDep outputFile) (edDeps ed)

lookupPackage :: Text -> IO (Text, Set Text)
lookupPackage "purescript-dom-indexed" = return ("https://github.com/slamdata/purescript-dom-indexed", mempty)
lookupPackage "purescript-fork" = return ("https://github.com/slamdata/purescript-fork", mempty)
lookupPackage name = do
  let url = "https://pursuit.purescript.org/packages/" ++ name
  req <- parseRequest $ unpack url
  doc <- httpSink req $ const sinkDoc
  --doc <- runConduitRes $ sourceFile "/Users/Michael/Desktop/foo.html" .| sinkDoc
  let cursor = fromDocument doc
      repos = cursor $// element "dt" >=> hasContent "Repository" >=> followingSibling &// element "a" >=> attribute "href"

  repo <-
    case repos of
      [] -> terror $ "Could not parse repo from: " ++ url
      x:_ -> return x

  let deps = cursor $// element "a" >=> attributeIs "class" "deplink__link" &// content
  return (repo, setFromList deps)

hasContent :: Text -> Axis
hasContent t c
  | T.strip (concat (c $// content)) == t = [c]
  | otherwise = []

getMasterCommit :: Text -> IO Text
getMasterCommit repo = do
  req <- parseRequest $ unpack repo
  res <- httpSink req $ const sinkDoc
  let cursor = fromDocument res
      oldStyle = cursor $// element "a" >=> attributeIs "class" "commit-tease-sha" >=> attribute "href"
      newStyle = cursor $// element "include-fragment" >=> attributeIs "class" "commit-tease commit-loader" >=> attribute "src"
  case oldStyle ++ newStyle of
    [x] -> return $ reverse $ takeWhile (/= '/') $ reverse x
    _ -> terror $ "Could not find commit from " ++ repo
    {-
  (user, proj) <- maybe (terror $ "Could not parse repo: " ++ repo) return $ do
    t1 <- stripPrefix "https://github.com/" repo
    let (user, t2) = break (== '/') t1
    t3 <- stripPrefix "/" t2
    return (user, takeWhile (/= '.') t3)
  let url = concat
        [ "https://api.github.com/repos/"
        , user
        , "/"
        , proj
        , "/branches"
        ]
  req <- parseRequest $ unpack url
  res <- httpJSON $ setRequestHeader "User-Agent" ["Purify AddDeps"]
                  $ setRequestHeader "Accept" ["application/json"] req
  maybe (terror $ "Unexpected result at: " ++ url) return $ start $ getResponseBody res
  where
    start (Array arr) = listToMaybe $ mapMaybe goObject $ toList arr
    start _ = Nothing

    goObject (Object o) = do
      "master" <- lookup "name" o
      Object o2 <- lookup "commit" o
      String sha <- lookup "sha" o2
      return sha
    goObject _ = Nothing
    -}
