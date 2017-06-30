{-# LANGUAGE OverloadedStrings #-}

module PI.RemoveFile
  (
    removeFile
  ) where

import           Periodic.Job   (Job, name, workDone)
import           PI.Utils       (doJobLater, unpackBS)

import           ShareFS.Client (Gateway, deleteFile)

removeFile :: Gateway -> Job -> IO ()
removeFile gw job = do
  ret <- deleteFile (unpackBS $ name job) gw
  case ret of
    Left err -> doJobLater job err
    Right _  -> workDone job
