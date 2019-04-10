{-# LANGUAGE OverloadedStrings #-}

module PI.SaveFile
  ( saveFile
  ) where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as B (writeFile)
import           Data.String            (fromString)
import           Periodic.Client        (ClientEnv, ClientM, runClientM,
                                         submitJob)
import           Periodic.Job           (JobM, name, workDone, workload)
import           Periodic.Types         (JobName (..))
import           System.FilePath        ((</>))

saveFile :: [String] -> ClientEnv -> FilePath -> JobM ()
saveFile fns env0 root = do
  n <- name
  jn <- name
  w <- workload
  liftIO $ B.writeFile (root </> n) w
  liftIO $ runClientM env0 $ do
    void $ submitJob "upload-next-guetzli" jn Nothing $ Just 300
    mapM_ (doSubmit jn) fns

  workDone

  where doSubmit :: JobName -> String -> ClientM ()
        doSubmit jn fn = void $ submitJob (fromString fn) jn Nothing Nothing
