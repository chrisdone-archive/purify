{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Strict
import           Data.Aeson
import           Data.Char
import           Data.Function
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Yaml
import           Options.Applicative.Simple
import           PackageInfo
import qualified Paths_purify
import           System.Directory
import           System.Exit
import qualified System.FilePath.Glob as Glob
import           System.Process

data Purify = Purify
  { outputFile :: FilePath
  , extraDeps :: [Dep]
  } deriving (Eq,Show)

instance ToJSON Purify where
  toJSON (Purify outFile deps) = object
    [ "output-file" .= outFile
    , "extra-deps" .= deps
    ]

instance FromJSON Purify where
  parseJSON j = do
    o <- parseJSON j
    outputFile <- o .: "output-file"
    extraDeps <-
      ((o .: "extra-deps") <|>
       fmap flattenDeps (o .: "extra-deps")) {-backwards compat-}
    pure (Purify {..})
    where
      flattenDeps :: Map.Map String Dep -> [Dep]
      flattenDeps = map (\(k, v) -> v { depName = k }) . Map.toList

data Dep = Dep
  { depRepo :: String
  , depCommit :: String
  , depName :: String
  , depModules :: Maybe [String]
  , depDeps :: [String]
  } deriving (Eq,Show)

instance ToJSON Dep where
  toJSON d = object
    (maybe id (\m -> (("modules" .= m):)) (depModules d)
    [ "repo" .= depRepo d
    , "commit" .= depCommit d
    , "name" .= depName d
    , "deps" .= depDeps d
    ])

instance FromJSON Dep where
  parseJSON j = do
    o <- parseJSON j
    repo <- o .: "repo"
    commit <- o .: "commit"
    let name' = takeWhile (/='.') (reverse (takeWhile (/='/') (reverse repo)))
    name <- o .:? "name" .!= name'
    mmodules <- o .:? "modules"
    deps <- o .:? "deps" .!= []
    pure (Dep repo commit name mmodules deps)

main :: IO ()
main = do
  exists <- doesFileExist "purify.yaml"
  if not exists
    then die "Expected purify.yaml in the directory of your PureScript project."
    else do
      result <- decodeFileEither "purify.yaml"
      case result of
        Left _ -> die "Couldn't parse purify.yaml file."
        Right config -> do
          args <- getArgs
          if null args
            then purify [] config False
            else join $ fmap snd $ simpleOptions
              $(simpleVersion Paths_purify.version)
              "purify build tool for PureScript"
              "Fully reproducible builds for PureScript"
              (pure ()) $ do
              addCommand "build" "Build code" id $ purify
                  <$> pure []
                  <*> pure config
              addCommand "ide" "Launch IDE interaction" id $ pure ide
              addCommand "add-deps" "Add dependencies to purify.yaml" id $ addDeps
                  <$> pure config
                  <*> some (strArgument (metavar "PACKAGE-NAME"))

data FetchState = Didn'tFetch | Fetched

purify :: [FilePath] -> Purify -> IO ()
purify inputFiles config = do
  createDirectoryIfMissing True ".purify-work/extra-deps"
  when
    (nub (extraDeps config) /= extraDeps config ||
     nubBy (on (==) depName) (extraDeps config) /= extraDeps config)
    (die "Dependencies contain duplicates.")
  mapM_
    (\dep -> do
       let depDir = getDepDir dep
           gitDir = depDir
       exists <- doesDirectoryExist depDir
       let clone =
             if not exists
               then do
                 putStrLn ("Cloning " ++ depName dep ++ " ...")
                 ok <- rawSystem "git" ["clone", "-q", depRepo dep, depDir]
                 case ok of
                   ExitFailure {} ->
                     die
                       ("Failed to clone package " ++
                        depName dep ++ " from " ++ depRepo dep)
                   _ -> checkout
               else checkout
           checkout = do
             tags <-
               fmap
                 lines
                 (readProcess
                    "git"
                    ["-C", gitDir, "tag", "--points-at", "HEAD"]
                    "")
             if any (== depCommit dep) tags
               then return ()
               else do
                 cur <- readProcess "git" ["-C", gitDir, "rev-parse", "HEAD"] ""
                 let commit = takeWhile isAlphaNum cur
                     shortDepCommit = take 7 (depCommit dep)
                 if commit == depCommit dep
                   then return ()
                   else do
                     fetch shortDepCommit Didn'tFetch
           fetch shortDepCommit fetchState = do
             case fetchState of
               Didn'tFetch ->
                 putStrLn
                   ("Checking out " ++
                    depName dep ++ " (" ++ shortDepCommit ++ ") ...")
               _ -> return ()
             res <-
               rawSystem
                 "git"
                 ["-C", gitDir, "checkout", "-f", "-q", depCommit dep]
             case res of
               ExitFailure {} ->
                 case fetchState of
                   Didn'tFetch -> do
                     putStrLn
                       ("Failed to checkout, fetching latest from remote ...")
                     fres <- rawSystem "git" ["-C", gitDir, "fetch", "-q"]
                     case fres of
                       ExitFailure {} ->
                         die
                           ("Tried to checkout " ++
                            depCommit dep ++
                            ", but it failed. Tried to fetch from the remote, but that failed too. Giving up.")
                       _ -> fetch shortDepCommit Fetched
                   Fetched ->
                     die
                       ("Checking out version failed for " ++
                        depName dep ++ ": " ++ depCommit dep)
               _ -> return ()
       clone)
    (extraDeps config)
  srcExists <- doesDirectoryExist "src/"
  if not srcExists
    then die
           "There is no src/ directory in this project. Please create one and put your PureScript files in there."
    else do
      let pattern = Glob.compile "**/*.purs"
      let dirs =
            map
              (++ "/src")
              ("." :
               map
                 getDepDir
                 (filter (isNothing . depModules) (extraDeps config)))
      foundPurs <- concat <$> mapM (Glob.globDir1 pattern) dirs
      let explicitPurs =
            concat
              (mapMaybe
                 (\dep -> do
                    modules <- depModules dep
                    pure
                      (map
                         (\modn -> getDepDir dep ++ "/" ++ topath modn)
                         modules))
                 (extraDeps config))
            where
              topath m = "src/" ++ replace m ++ ".purs"
              replace ('.':cs) = '/' : replace cs
              replace (c:cs) = c : replace cs
              replace [] = []
      let allPurs = inputFiles ++ foundPurs ++ explicitPurs
      putStrLn ("Compiling " ++ show (length allPurs) ++ " modules ...")
      let outputDir = ".purify-work/js-output"
      status <- rawSystem "purs" (["compile", "-o", outputDir] ++ allPurs)
      case status of
        ExitFailure {} -> die "Compile failed."
        _ -> do
          putStrLn "Bundling ..."
          stat <-
            rawSystem
              "purs"
              [ "bundle"
              , ".purify-work/js-output/**/*.js"
              , "-m"
              , "Main"
              , "--main"
              , "Main"
              , "-o"
              , outputFile config
              ]
          case stat of
            ExitFailure {} -> die "Bundling failed."
            _ -> putStrLn ("Output bundled to " ++ outputFile config)
  where
    getDepDir dep = ".purify-work/extra-deps/" ++ depName dep

ide :: IO ()
ide = rawSystem
  "purs"
  ["ide","server", "--output-directory", ".purify-work/js-output"
  ,".purify-work/extra-deps/*/src/**/*.purs", "src/**/*.purs"]
  >>= exitWith

addDeps :: Purify -> [String] -> IO ()
addDeps (Purify outFile deps) newDeps =
  void (runStateT (mapM_ (addDep outFile []) newDeps) depsMap)
  where
  depsMap = Map.unions (map (\dep -> Map.singleton (depName dep) dep) deps)

addDep :: FilePath -- ^ out file
       -> [String] -- ^ call stack, to avoid cycles
       -> String -- ^ new dep
       -> StateT (Map.Map String Dep) IO ()
addDep _ depStack newDep
  | newDep `elem` depStack = error ("Dep cycle detected: " ++ show (newDep : depStack))
addDep outFile depStack newDep = do
  let newStack = newDep : depStack
  allDeps <- get
  case Map.lookup newDep allDeps of
    Nothing -> do
      liftIO (putStrLn ("Adding dep: " ++ newDep))
      (repo, deps) <- liftIO (lookupPackage newDep)
      master <- liftIO (getMasterCommit repo)
      modify (Map.insert newDep (Dep
        { depRepo = repo
        , depCommit = master
        , depName = newDep
        , depModules = Nothing
        , depDeps = deps
        }))
      deps' <- get
      liftIO (encodeFile "purify.yaml" (Purify outFile (Map.elems deps')))
      mapM_ (addDep outFile newStack) deps
    Just ed -> mapM_ (addDep outFile newStack) (depDeps ed)
