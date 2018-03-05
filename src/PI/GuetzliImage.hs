{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PI.GuetzliImage
  (
    GuetzliConfig (..)
  , guetzliImage
  , defaultGuetzliConfig
  ) where

import           Control.Monad                  (void)
import           Control.Monad.IO.Class         (liftIO)
import           Data.Aeson                     (FromJSON, parseJSON,
                                                 withObject, (.!=), (.:?))
import           Data.Int                       (Int64)
import           Periodic.Client                (ClientEnv, runClientT,
                                                 submitJob)
import           Periodic.Job                   (JobT, name, workDone)
import           Periodic.Types                 (JobName (..))
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

guetzliImage :: GuetzliConfig -> ClientEnv IO -> Gateway -> JobT IO ()
guetzliImage GuetzliConfig{..} env0 gw =
  getFileAndNext gw $ \bs -> do
    (code, out, _) <- liftIO $ readProcessWithExitCode guetzliCommand ["-", "/dev/stdout"] bs
    case code of
      ExitFailure _ -> doJobLater "guetzli failed"
      ExitSuccess   -> do
        n <- name
        let outFileName = guetzliOutput </> takeFileName n
            outFileName' = JobName $ packBS outFileName
        putFileAndNext gw outFileName out $ do
          liftIO $ runClientT env0 $ do
            void $ submitJob "upload" outFileName' 0
            void $ submitJob "remove" outFileName' guetzliDelay
          workDone
