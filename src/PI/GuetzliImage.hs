{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PI.GuetzliImage
  (
    GuetzliConfig (..)
  , guetzliImage
  , defaultGuetzliConfig
  ) where

import           Data.Aeson                     (FromJSON, parseJSON,
                                                 withObject, (.!=), (.:?))
import           Data.Int                       (Int64)
import           Periodic.Client                (Client, submitJob)
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

guetzliImage :: GuetzliConfig -> Client -> Gateway -> Job -> IO ()
guetzliImage (GuetzliConfig {..}) c gw job = do
  getFileAndNext gw job $ \bs -> do
    (code, out, err) <- readProcessWithExitCode guetzliCommand ["-", "/dev/stdout"] bs
    case code of
      ExitFailure _ -> doJobLater job "guetzli failed"
      ExitSuccess   ->
        putFileAndNext gw job outFileName out $ do
          submitJob c "upload" outFileName' 0
          submitJob c "remove" outFileName' guetzliDelay
          workDone job

  where outFileName = guetzliOutput </> takeFileName (unpackBS $ name job)
        outFileName' = packBS outFileName
