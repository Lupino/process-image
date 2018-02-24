module PI.RemoveFile
  (
    removeFile
  ) where

import           Periodic.Job   (Job, name, workDone)
import           Periodic.Monad (unsafeLiftIO)
import           PI.Utils       (doJobLater)

import           ShareFS.Client (Gateway, deleteFile)

removeFile :: Gateway -> Job ()
removeFile gw = do
  n <- name
  ret <- unsafeLiftIO $ deleteFile n gw
  case ret of
    Left err -> doJobLater err
    Right _  -> workDone
