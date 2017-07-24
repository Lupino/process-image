{-# LANGUAGE OverloadedStrings #-}

module PI.RemoveFile
  (
    removeFile
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Periodic.Job           (Job, name, workDone)
import           PI.Utils               (doJobLater, unpackBS)

import           ShareFS.Client         (Gateway, deleteFile)

removeFile :: Gateway -> Job ()
removeFile gw = do
  n <- unpackBS <$> name
  ret <- liftIO $ deleteFile n gw
  case ret of
    Left err -> doJobLater err
    Right _  -> workDone
