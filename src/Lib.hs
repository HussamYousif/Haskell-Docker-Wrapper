{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  ) where

import qualified Data.Foldable as Foldable
import System.Directory
import System.Environment
import System.Process

repos = "./repos"

data Service' =
  Service
    { containerId :: String
    , name :: String
    }

data DockerService =
  Service
    {
    }

someFunc :: IO ()
someFunc = do
  args <- getArgs
  parseArgs args

startServices :: IO ()
startServices = do
  fileContent <- readFile repos
  let repositories = lines fileContent
  Foldable.forM_ repositories runService
  putStrLn "Success"

runService :: String -> IO ()
runService path = do
  setCurrentDirectory path
  callCommand "docker-compose up --build -d"
  putStrLn path

parseArgs :: [String] -> IO ()
parseArgs ("run":_) = startServices
parseArgs _ = error "Can not parse the arguments given"

parseServices
