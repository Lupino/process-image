module PI.RemoveFile
  ( removeFile
  ) where

import           Control.Monad          (when)
import           Control.Monad.IO.Class (liftIO)
import           Periodic.Job           (JobM, name, workDone)
import qualified System.Directory       as D (doesFileExist, removeFile)
import           System.FilePath        ((</>))

removeFile :: FilePath -> JobM ()
removeFile root = do
  n <- name
  exists <- liftIO $ D.doesFileExist $ root </> n
  when exists $ liftIO $ D.removeFile $ root </> n
  workDone
