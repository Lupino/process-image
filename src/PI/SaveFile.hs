{-# LANGUAGE OverloadedStrings #-}

module PI.SaveFile
  ( saveFile
  ) where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as B (writeFile)
import           Data.String            (fromString)
import           Periodic.Job           (JobM, name, submitJob, workDone,
                                         workload)
import           Periodic.Types         (JobName (..))
import           System.FilePath        ((</>))

saveFile :: [String] -> FilePath -> JobM ()
saveFile fns root = do
  n <- name
  jn <- name
  w <- workload
  liftIO $ B.writeFile (root </> n) w
  void $ submitJob "upload-next-guetzli" jn "" 300 0
  mapM_ (doSubmit jn) fns

  void workDone

  where doSubmit :: JobName -> String -> JobM ()
        doSubmit jn fn = void $ submitJob (fromString fn) jn  "" 0 0
