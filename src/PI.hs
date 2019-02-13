{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PI
  ( module X
  , initialWorker
  ) where

import           PI.Config             as X
import           PI.GuetzliImage       as X
import           PI.RemoveFile         as X
import           PI.ResizeImage        as X

import           Periodic.Client       (ClientEnv)
import           Periodic.Types        (FuncName (..))
import           Periodic.Worker       (WorkerT, addFunc)

import           Control.Monad         (when)
import           Data.ByteString.Char8 (pack)
import           PI.SaveFile

initialWorker :: ClientEnv -> Config -> WorkerT IO ()
initialWorker env0 Config{..} = do
  when enableSave $ addFunc "save" $ saveFile root
  when enableRemove $ addFunc "remove" $ removeFile root
  when enableGuetzli $ addFunc "guetzli" $ guetzliImage guetzliConfig env0 root
  mapM_ initialResizeImage resizesConfig

  where initialResizeImage :: ResizeConfig -> WorkerT IO ()
        initialResizeImage conf = addFunc (FuncName $ funcName conf) $ resizeImage conf env0 root
        funcName = pack . imageFuncName
