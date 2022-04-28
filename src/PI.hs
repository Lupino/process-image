{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PI
  ( initialWorker
  , module PI.Config
  ) where

import           Control.Monad          (when)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString.Char8  (pack)
import           Periodic.Types         (FuncName (..))
import           Periodic.Worker        (WorkerM, addFunc)
import           PI.Config
import           PI.GuetzliImage
import           PI.RemoveFile
import           PI.ResizeImage
import           PI.SaveFile
import           System.Directory       (createDirectoryIfMissing)
import           System.FilePath        ((</>))

initialWorker :: Config -> WorkerM ()
initialWorker Config{..} = do
  liftIO $ do
    createDirectoryIfMissing True root
    createDirectoryIfMissing True $ root </>  guetzliOutput guetzliConfig
  when enableSave $ addFunc "save" $ saveFile (map imageFuncName resizesConfig) root
  when enableRemove $ addFunc "remove" $ removeFile root
  when enableGuetzli $ addFunc "guetzli" $ guetzliImage guetzliConfig root
  mapM_ initialResizeImage resizesConfig

  where initialResizeImage :: ResizeConfig -> WorkerM ()
        initialResizeImage conf = do
          liftIO $ createDirectoryIfMissing True $ root </> imageOutput conf
          addFunc (FuncName $ funcName conf) $ resizeImage conf root
        funcName = pack . imageFuncName
