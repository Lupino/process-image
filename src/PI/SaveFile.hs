{-# LANGUAGE OverloadedStrings #-}

module PI.SaveFile
  ( saveFile
  ) where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as B (writeFile)
import           Data.String            (fromString)
import           Periodic.Client        (ClientEnv, ClientT, runClientT,
                                         submitJob)
import           Periodic.Job           (JobT, name, workDone, workload)
import           Periodic.Types         (FuncName (..), JobName (..))
import           System.FilePath        ((</>))

saveFile :: [String] -> ClientEnv -> FilePath -> JobT IO ()
saveFile fns env0 root = do
  n <- name
  jn <- name
  w <- workload
  liftIO $ B.writeFile (root </> n) w
  liftIO $ runClientT env0 $ do
    void $ submitJob "remove" jn Nothing $ Just 43200
    void $ submitJob "upload-next-guetzli" jn Nothing Nothing
    mapM_ (doSubmit jn) fns

  workDone

  where doSubmit :: JobName -> String -> ClientT IO ()
        doSubmit jn fn = void $ submitJob (fromString fn) jn Nothing Nothing
