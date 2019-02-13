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
import           Data.String            (fromString)
import           Periodic.Client        (ClientEnv, runClientT, submitJob)
import           Periodic.Job           (JobT, name, withLock, workDone)
import           Periodic.Types         (JobName (..), LockName (..))
import           PI.Utils
import           System.Exit            (ExitCode (..))
import           System.FilePath        (takeFileName, (</>))
import           System.Process         (rawSystem)


data GuetzliConfig = GuetzliConfig { guetzliCommand :: FilePath
                                   , guetzliOutput  :: FilePath
                                   , guetzliDelay   :: Int64
                                   , guetzliLName   :: Maybe String
                                   , guetzliLCount  :: Int
                                   }
  deriving (Show)

instance FromJSON GuetzliConfig where
  parseJSON = withObject "GuetzliConfig" $ \o -> do
    guetzliCommand <- o .:? "command" .!= "guetzli"
    guetzliOutput  <- o .:? "output"  .!= "guetzli"
    guetzliDelay   <- o .:? "delay"   .!= 432000
    guetzliLName   <- o .:? "lock-name"
    guetzliLCount  <- o .:? "lock-count" .!= 1
    return GuetzliConfig {..}

defaultGuetzliConfig :: GuetzliConfig
defaultGuetzliConfig = GuetzliConfig { guetzliCommand = "guetzli"
                                     , guetzliOutput = "guetzli"
                                     , guetzliDelay = 432000
                                     , guetzliLName = Nothing
                                     , guetzliLCount = 1
                                     }

guetzliImage' :: GuetzliConfig -> ClientEnv -> FilePath -> JobT IO ()
guetzliImage' GuetzliConfig{..} env0 root = do
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

guetzliImage :: GuetzliConfig -> ClientEnv -> FilePath -> JobT IO ()
guetzliImage c@GuetzliConfig{..} env0 = f guetzliLName . guetzliImage' c env0
  where f :: Maybe String -> JobT IO () -> JobT IO ()
        f Nothing  = id
        f (Just n) = withLock (LockName $ fromString n) guetzliLCount
