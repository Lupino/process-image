{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PI
  ( initialWorker
  , module PI.Config
  ) where

import           Control.Monad          (void, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString.Char8  (pack)
import           Periodic.Types         (FuncName (..))
import           Periodic.Worker        (WorkerM, addFunc)
import           PI.Config
import           PI.GuetzliImage
import           PI.RemoveFile
import           PI.ResizeImage
import           PI.SaveFile
import           PI.UploadFile
import           System.Directory       (createDirectoryIfMissing)
import           System.FilePath        ((</>))

initialWorker :: Config -> WorkerM ()
initialWorker Config{..} = do
  liftIO $ do
    createDirectoryIfMissing True root
    createDirectoryIfMissing True $ root </>  guetzliOutput guetzliConfig
    void $ initBucket bucketConfig
  when enableSave $ void $ addFunc "save" $ saveFile (map imageFuncName resizesConfig) root
  when enableRemove $ void $ addFunc "remove" $ removeFile root
  when enableGuetzli $ void $ addFunc "guetzli" $ guetzliImage guetzliConfig root
  mapM_ initialResizeImage resizesConfig
  void $ addFunc "upload" $ uploadImage remotePath root
  void $ addFunc "upload-next-remove" $ uploadNextRemove remotePath root
  void $ addFunc "upload-next-guetzli" $ uploadNextGuetzli remotePath root

  where initialResizeImage :: ResizeConfig -> WorkerM ()
        initialResizeImage conf = do
          liftIO $ createDirectoryIfMissing True $ root </> imageOutput conf
          void $ addFunc (FuncName $ funcName conf) $ resizeImage conf root
        funcName = pack . imageFuncName
        remotePath = bucketPath bucketConfig
