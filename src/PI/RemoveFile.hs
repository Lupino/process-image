module PI.RemoveFile
  (
    removeFile
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Periodic.Job           (JobT, name, workDone)
import           PI.Utils               (doJobLater)

import           ShareFS.Client         (Gateway, deleteFile)

removeFile :: Gateway -> JobT IO ()
removeFile gw = do
  n <- name
  ret <- liftIO $ deleteFile n gw
  case ret of
    Left err -> doJobLater err
    Right _  -> workDone
