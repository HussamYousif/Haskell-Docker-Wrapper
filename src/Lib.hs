{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  ) where

import Control.Monad
import Data.Char
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Data.List.Split
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process
import Text.Printf

repos = "./repos"

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

-- TODO Make it run one service if specified.
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
