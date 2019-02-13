module PI.SaveFile
  ( saveFile
  ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as B (writeFile)
import           Periodic.Job           (JobT, name, workDone, workload)
import           System.FilePath        ((</>))

saveFile :: FilePath -> JobT IO ()
saveFile root = do
  n <- name
  w <- workload
  liftIO $ B.writeFile (root </> n) w
  workDone
