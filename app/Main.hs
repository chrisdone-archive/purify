{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Char
import Data.Function
import Data.List
import Data.Yaml
import System.Directory
import System.Environment
import System.Exit
import System.Process

data Purify = Purify
  { outputFile :: FilePath
  , extraDeps :: [Dep]
  } deriving (Eq,Show)

instance FromJSON Purify where
  parseJSON j = do
    o <- parseJSON j
    outputFile <- o .: "output-file"
    extraDeps <- o .: "extra-deps"
    pure (Purify {..})

data Dep = Dep
  { depRepo :: String
  , depCommit :: String
  , depName :: String
  } deriving (Eq,Show)

instance FromJSON Dep where
  parseJSON j = do
    o <- parseJSON j
    repo <- o .: "repo"
    commit <- o .: "commit"
    let name = takeWhile (/='.') (reverse (takeWhile (/='/') (reverse repo)))
    pure (Dep repo commit name)

main :: IO ()
main = do
  exists <- doesFileExist "purify.yaml"
  if not exists
    then error
           "Expected purify.yaml in the directory of your PureScript project."
    else do
      result <- decodeFileEither "purify.yaml"
      case result of
        Left e -> error (displayException e)
        Right config -> do
          files <- fmap (filter (not . isPrefixOf "-")) getArgs
          purify files config

data FetchState = Didn'tFetch | Fetched

purify :: [FilePath] -> Purify -> IO ()
purify inputFiles config = do
  createDirectoryIfMissing True ".purify-work/extra-deps"
  when
    (nub (extraDeps config) /= extraDeps config ||
     nubBy (on (==) depName) (extraDeps config) /= extraDeps config)
    (error "Dependencies contain duplicates.")
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
                     error
                       ("Failed to clone package " ++
                        depName dep ++ " from " ++ depRepo dep)
                   _ -> checkout
               else checkout
           checkout = do
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
                         error
                           ("Tried to checkout " ++
                            depCommit dep ++
                            ", but it failed. Tried to fetch from the remote, but that failed too. Giving up.")
                       _ -> fetch shortDepCommit Fetched
                   Fetched ->
                     error
                       ("Checking out version failed for " ++
                        depName dep ++ ": " ++ depCommit dep)
               _ -> return ()
       clone)
    (extraDeps config)
  srcExists <- doesDirectoryExist "src/"
  if not srcExists
    then error
           "There is no src/ directory in this project. Please create one and put your PureScript files in there."
    else do
      let args =
            map (++ "/src") ("." : map getDepDir (extraDeps config)) ++
            ["-name", "*.purs"]
      allPurs <- fmap (++ inputFiles) (fmap lines (readProcess "find" args ""))
      putStrLn ("Compiling " ++ show (length allPurs) ++ " modules ...")
      let outputDir = ".purify-work/js-output"
      status <-
        rawSystem "stack" (["exec", "--", "psc", "-o", outputDir] ++ allPurs)
      case status of
        ExitFailure {} -> error "Compile failed."
        _ -> do
          putStrLn "Bundling ..."
          stat <-
            rawSystem
              "stack"
              [ "exec"
              , "--"
              , "psc-bundle"
              , ".purify-work/js-output/**/*.js"
              , "-m"
              , "Main"
              , "--main"
              , "Main"
              , "-o"
              , outputFile config
              ]
          case stat of
            ExitFailure {} -> error "Bundling failed."
            _ -> putStrLn ("Output bundled to " ++ outputFile config)
  where
    getDepDir dep = ".purify-work/extra-deps/" ++ depName dep
