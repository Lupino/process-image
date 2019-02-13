{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PI.GuetzliImage
  ( GuetzliConfig (..)
  , guetzliImage
  , defaultGuetzliConfig
  ) where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON, parseJSON, withObject, (.!=),
                                         (.:?))
import           Data.Int               (Int64)
import           Periodic.Client        (ClientEnv, runClientT, submitJob)
import           Periodic.Job           (JobT, name, workDone)
import           Periodic.Types         (JobName (..))
import           PI.Utils
import           System.Exit            (ExitCode (..))
import           System.FilePath        (takeFileName, (</>))
import           System.Process         (rawSystem)


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

guetzliImage :: GuetzliConfig -> ClientEnv -> FilePath -> JobT IO ()
guetzliImage GuetzliConfig{..} env0 root = do
  fn <- name
  let outFileName = guetzliOutput </> takeFileName fn
      outFileName' = JobName $ packBS outFileName
  code <- liftIO $ rawSystem guetzliCommand [root </> fn, root </> outFileName]

  case code of
    ExitFailure _ -> doJobLater "guetzli failed"
    ExitSuccess   -> do
      liftIO $ runClientT env0 $ do
        void $ submitJob "upload" outFileName' Nothing Nothing
        void $ submitJob "remove" outFileName' Nothing $ Just guetzliDelay
      workDone
