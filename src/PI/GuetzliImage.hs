{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PI.GuetzliImage
  (
    GuetzliConfig (..)
  , guetzliImage
  , defaultGuetzliConfig
  ) where

import           Control.Monad.IO.Class         (liftIO)
import           Data.Aeson                     (FromJSON, parseJSON,
                                                 withObject, (.!=), (.:?))
import           Data.Int                       (Int64)
import           Periodic.Client                (Connection, runClient_,
                                                 submitJob)
import           Periodic.Job                   (Job, name, workDone)
import           PI.Utils
import           ShareFS.Client                 (Gateway)
import           System.Exit                    (ExitCode (..))
import           System.FilePath                (takeFileName, (</>))
import           System.Process.ByteString.Lazy (readProcessWithExitCode)


data GuetzliConfig = GuetzliConfig { guetzliCommand :: FilePath
                                   , guetzliOutput  :: FilePath
                                   , guetzliDelay   :: Int64
                                   }
  deriving (Show)

instance FromJSON GuetzliConfig where
  parseJSON = withObject "GuetzliConfig" $ \o -> do
    guetzliCommand <- o .:? "command" .!= "guetzli"
    guetzliOutput  <- o .:? "output"  .!= "guetzli"
    guetzliDelay   <- o .:? "delay"   .!= 432000
    return GuetzliConfig {..}

defaultGuetzliConfig :: GuetzliConfig
defaultGuetzliConfig = GuetzliConfig { guetzliCommand = "guetzli"
                                     , guetzliOutput = "guetzli"
                                     , guetzliDelay = 432000
                                     }

guetzliImage :: GuetzliConfig -> Connection -> Gateway -> Job ()
guetzliImage GuetzliConfig{..} c gw =
  getFileAndNext gw $ \bs -> do
    (code, out, err) <- liftIO $ readProcessWithExitCode guetzliCommand ["-", "/dev/stdout"] bs
    case code of
      ExitFailure _ -> doJobLater "guetzli failed"
      ExitSuccess   -> do
        n <- unpackBS <$> name
        let outFileName = guetzliOutput </> takeFileName n
            outFileName' = packBS outFileName
        putFileAndNext gw outFileName out $ do
          liftIO $ runClient_ c $ do
            submitJob "upload" outFileName' 0
            submitJob "remove" outFileName' guetzliDelay
          workDone
