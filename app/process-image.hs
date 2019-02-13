module Main
  (
    main
  ) where

import           PI                 (Config (..), initialWorker)

import           Data.Yaml          (decodeFileThrow)
import           System.Environment (getArgs)

import           Periodic.Client    (close, open, runClientT)
import           Periodic.Worker    (runWorkerT, work)

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
  clientEnv <- open return host
  runWorkerT return host $ do
    initialWorker clientEnv config
    work thread

  runClientT clientEnv close
