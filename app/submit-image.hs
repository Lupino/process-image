{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Periodic.Client     (Client, newClient, submitJob)
import qualified Periodic.Client     as Client (close)
import           Periodic.Socket     (connectTo)
import           Periodic.Transport  (makeSocketTransport)

import           Control.Monad       (void)
import           Data.String.Utils   (split)

import           Data.Semigroup      ((<>))
import           Options.Applicative

import           Data.ByteString     (ByteString)
import           Data.Text           as T (pack)
import           Data.Text.Encoding  (encodeUtf8)

data Options = Options { periodicHost :: String
                       , periodicPort :: Int
                       , funcNameList :: String
                       }

parser :: Parser Options
parser = Options <$> strOption (long "host"
                                <> short 'H'
                                <> metavar "HOST"
                                <> help "Periodic Host"
                                <> value "127.0.0.1")
                 <*> option auto (long "port"
                                <> short 'P'
                                <> metavar "PORT"
                                <> help "Periodic Port"
                                <> value 5000)
                 <*> strOption (long "func"
                                <> short 'f'
                                <> metavar "FUNC"
                                <> help "FuncName List"
                                <> value "")

main :: IO ()
main = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      ( fullDesc
     <> progDesc "Submit Image"
     <> header "submit-image - Submit Image" )

program :: Options -> IO ()
program (Options { periodicPort = port
                 , periodicHost = host
                 , funcNameList = funcs
                 }) = do

  c <- newClient =<< makeSocketTransport =<< connectTo (Just host) (show port)
  name <- getLine
  mapM (doSubmit c name) $ split "," funcs
  submitJob c "remove" (packBS name) 43200
  Client.close c
  putStrLn "OK"

doSubmit :: Client -> FilePath -> String -> IO ()
doSubmit c fileName funcName = void $ submitJob c (packBS funcName) (packBS fileName) 0

packBS :: String -> ByteString
packBS = encodeUtf8 . T.pack

