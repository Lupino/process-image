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
import           Data.String            (fromString)
import           Periodic.Client        (ClientEnv, runClientT, submitJob)
import           Periodic.Job           (JobT, count, name, schedLater',
                                         withLock, workDone)
import           Periodic.Types         (LockName (..))
import           System.Exit            (ExitCode (..))
import           System.FilePath        (takeFileName, (</>))
import           System.Log.Logger      (errorM)
import           System.Process         (rawSystem)


data GuetzliConfig = GuetzliConfig { guetzliCommand :: FilePath
                                   , guetzliOutput  :: FilePath
                                   , guetzliLName   :: Maybe String
                                   , guetzliLCount  :: Int
                                   }
  deriving (Show)

instance FromJSON GuetzliConfig where
  parseJSON = withObject "GuetzliConfig" $ \o -> do
    guetzliCommand <- o .:? "command" .!= "guetzli"
    guetzliOutput  <- o .:? "output"  .!= "guetzli"
    guetzliLName   <- o .:? "lock-name"
    guetzliLCount  <- o .:? "lock-count" .!= 1
    return GuetzliConfig {..}

defaultGuetzliConfig :: GuetzliConfig
defaultGuetzliConfig = GuetzliConfig { guetzliCommand = "guetzli"
                                     , guetzliOutput = "guetzli"
                                     , guetzliLName = Nothing
                                     , guetzliLCount = 1
                                     }

guetzliImage' :: GuetzliConfig -> ClientEnv -> FilePath -> JobT IO ()
guetzliImage' GuetzliConfig{..} env0 root = do
  fn <- name
  let outFileName = guetzliOutput </> takeFileName fn
  code <- liftIO $ rawSystem guetzliCommand [root </> fn, root </> outFileName]

  case code of
    ExitFailure _ -> do
      liftIO $ errorM "PI.GuetzliImage" "guetzli failed"
      c <- count
      if c > 15 then workDone
                else schedLater' (fromIntegral $ later c) 1

    ExitSuccess   -> do
      liftIO
        . runClientT env0 $ do
          void $ submitJob "remove" (fromString fn) Nothing $ Just 300
          void $ submitJob "upload-next-remove" (fromString outFileName) Nothing Nothing

      workDone

  where later :: Int -> Int
        later c = c * (10 + c)

guetzliImage :: GuetzliConfig -> ClientEnv -> FilePath -> JobT IO ()
guetzliImage c@GuetzliConfig{..} env0 = f guetzliLName . guetzliImage' c env0
  where f :: Maybe String -> JobT IO () -> JobT IO ()
        f Nothing  = id
        f (Just n) = withLock (LockName $ fromString n) guetzliLCount
