module Main
  (
    main
  ) where

import           Data.Yaml          (decodeFileThrow)
import           Periodic.Worker    (startWorkerM, work)
import           PI                 (Config (..), initialWorker)
import           System.Environment (getArgs)

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
  startWorkerM host $ do
    initialWorker config
    work thread
