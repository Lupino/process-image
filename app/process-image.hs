module Main
  (
    main
  ) where

import           PI                 (Config (..), initialGateway, initialWorker)

import           Data.Yaml          (decodeFile)
import           System.Environment (getArgs)

import           Periodic.Client    (Client, newClient)
import qualified Periodic.Client    as Client (close)
import           Periodic.Socket    (connectTo)
import           Periodic.Worker    (newWorker, work)

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
program config@(Config { periodicHost = host, periodicPort = port, threadNum = thread }) = do
  gw <- initialGateway config
  c <- newClient =<< connectTo host (show port)
  w <- newWorker =<< connectTo host (show port)
  initialWorker w c gw config
  work w thread
  Client.close c
