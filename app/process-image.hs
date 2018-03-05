module Main
  (
    main
  ) where

import           PI                 (Config (..), initialGateway, initialWorker)

import           Data.Yaml          (decodeFile)
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

  c <- decodeFile configFile :: IO (Maybe Config)
  case c of
    Nothing     -> putStrLn "Config file format error"
    Just config -> program config

program :: Config -> IO ()
program config@Config{periodicHost = host, threadNum = thread} = do
  gw <- initialGateway config
  clientEnv <- open return host
  runWorkerT return host $ do
    initialWorker clientEnv gw config
    work thread

  runClientT clientEnv close
