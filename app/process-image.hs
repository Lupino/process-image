module Main
  (
    main
  ) where

import           PI                 (Config (..), initialWorker)

import           Data.Yaml          (decodeFileThrow)
import           System.Environment (getArgs)

import           Periodic.Client    (close, open, runClientM)
import           Periodic.Worker    (runWorkerM, work)

defaultConfigFile :: FilePath
defaultConfigFile = "config.yml"

main :: IO ()
main = do
  args <- getArgs

  let configFile = case args of
                     [x] -> x
                     _   -> defaultConfigFile

  c <- decodeFileThrow configFile
  program c

program :: Config -> IO ()
program config@Config{periodicHost = host, threadNum = thread} = do
  clientEnv <- open host
  runWorkerM host $ do
    initialWorker clientEnv config
    work thread

  runClientM clientEnv close
