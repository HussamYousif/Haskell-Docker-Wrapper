{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  ) where

import Control.Monad
import Data.Char
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process
import Text.Printf

repos = "./repos"

services = "./services"

data Flag
  = Run
  | Log
  | Enter
  | Help
  deriving (Eq, Ord, Enum, Show, Bounded)

data DockerService =
  Service
    { id :: String
    , name :: String
    }

flags =
  [ Option ['r'] ["run"] (NoArg Run) "Runs all the docker services"
  , Option
      ['l']
      ["log"]
      (NoArg Log)
      "prints out the log of the service given. Arguement service"
  , Option
      ['e']
      ["enter"]
      (NoArg Enter)
      "Enter the shell within the container specified by an argument. Argument service"
  , Option [] ["help"] (NoArg Help) "Print this help message"
  ]

-- Docker has to be running to reach this stage
dockerPS :: IO [String]
dockerPS = do
  (_, stdin, stderr) <- runDockerCommand ["ps"]
  let services = (tail . lines) stdin
  return services

-- Reads a file and returns a tuple of the service and the alias given to the service
-- by the user.
-- reads the "./services" file
readServices :: IO [(String, String)]
readServices = do
  servicesContent <- readFile services
  let servicesContent' = (lines) servicesContent -- Remove the first line which contains CONTAINERID etc.
  let servicesContent'' = map words servicesContent'
  let results = map (\line -> (line !! 0, line !! 1)) servicesContent''
  return results

-- slices the string
getContainerId :: String -> String
getContainerId psLine = take 12 psLine

getServiceData = do
  runningServices <- dockerPS
  userServices <- readServices
  putStrLn "Hei"

-- Error here.
getDockerService :: [String] -> ([String], [String]) -> [DockerService]
getDockerService _ [] = []
getDockerService runningServices ((a, b):ys) =
  if service /= []
    then (Service a (getContainerId service)) :
         getServiceData runningServices (ys)
    else getServiceData runningServices (ys)
  where
    service = filter (\s -> elem b s)

parseArgs :: [String] -> IO (Flag, Maybe String)
parseArgs argv =
  case getOpt Permute flags argv of
    ([argument], [service], []) -> return (argument, Just service)
    ([argument], [], []) -> return (argument, Nothing)
    (argList, serviceList, []) ->
      ioError (userError "Multiple arguments or services.")
    (_, _, errs) -> do
      ioError (userError (concat errs ++ usageInfo header flags))
  where
    header = "Usage: Haskell-Docker-Wrapper [-rle] [service]"

someFunc :: IO ()
someFunc = do
  dockerIsRunning <- isDockerRunning
  -- Stop execution if docker is not running.
  if not dockerIsRunning
    then putStrLn "Docker is not running"
    else do
      userInput <- getContents
      (argument, service) <- (parseArgs . words) userInput
      case (argument, service) of
        (Run, Nothing) -> startServices -- Start all services
      putStrLn "Haskell-Docker-Wrapper Exit"

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

-- The docker arguments must be encapsulated in a list
runDockerCommand :: [String] -> IO (ExitCode, String, String)
runDockerCommand args = readProcessWithExitCode "docker" args []

isDockerRunning :: IO Bool
isDockerRunning = do
  (exit, _, _) <- runDockerCommand ["ps"]
  case exit of
    ExitSuccess -> return True
    _ -> return False
