{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Periodic.Client     (Client, close, runClient, submitJob)

import           Control.Monad       (void)
import           Data.String.Utils   (split)

import           Data.Semigroup      ((<>))
import           Options.Applicative

import           Data.ByteString     (ByteString)
import           Data.Text           as T (pack)
import           Data.Text.Encoding  (encodeUtf8)

data Options = Options { periodicHost :: String
                       , funcNameList :: String
                       }

parser :: Parser Options
parser = Options <$> strOption (long "host"
                                <> short 'H'
                                <> metavar "HOST"
                                <> help "Periodic Host"
                                <> value "unix:///tmp/periodic.sock")
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
program (Options { periodicHost = host
                 , funcNameList = funcs
                 }) = do

  name <- getLine
  runClient return host $ do
    mapM (doSubmit name) $ split "," funcs
    submitJob "remove" (packBS name) 43200
    close

  putStrLn "OK"

doSubmit :: FilePath -> String -> Client ()
doSubmit fileName funcName = void $ submitJob (packBS funcName) (packBS fileName) 0

packBS :: String -> ByteString
packBS = encodeUtf8 . T.pack
