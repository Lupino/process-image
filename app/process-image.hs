module Main
  (
    main
  ) where

import           PI                 (Config (..), initialGateway, initialWorker)

import           Data.Yaml          (decodeFile)
import           System.Environment (getArgs)

import           Periodic.Client    (close, open, runClient_)
import           Periodic.Worker    (runWorker, work)

defaultConfigFile :: FilePath
defaultConfigFile = "config.yml"

main :: IO ()
main = do
  args <- getArgs

  let configFile = case args of
                     [x] -> x
                     _   -> defaultConfigFile

  c <- decodeFile configFile :: IO (Maybe Config)
  case c of
    Nothing     -> putStrLn "Config file format error"
    Just config -> program config

program :: Config -> IO ()
program config@(Config { periodicHost = host, threadNum = thread }) = do
  gw <- initialGateway config
  c <- open return host
  runWorker return host $ do
    initialWorker c gw config
    work thread

  runClient_ c close
