{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PI
  (
    module X
  , initialGateway
  , initialWorker
  ) where

import           PI.Config             as X
import           PI.GuetzliImage       as X
import           PI.RemoveFile         as X
import           PI.ResizeImage        as X

import           ShareFS.Client        (Gateway (..), initMgr)

import           Periodic.Client       (ClientEnv)
import           Periodic.Types        (FuncName (..))
import           Periodic.Worker       (WorkerT, addFunc)

import           Control.Monad         (when)
import           Data.ByteString.Char8 (pack)

initialGateway :: Config -> IO Gateway
initialGateway Config{..} = initMgr Gateway
  { getGWUri       = shareFSHost
  , getGWAppKey    = shareFSKey
  , getGWAppSecret = shareFSSecret
  , getGWTimeout   = 1000
  , getGWConnCount = 100
  , getGWMgr       = Nothing
  }

initialWorker :: ClientEnv IO -> Gateway -> Config -> WorkerT IO ()
initialWorker env0 gw Config{..} = do
  when enableRemove $ addFunc "remove" $ removeFile gw
  when enableGuetzli $ addFunc "guetzli" $ guetzliImage guetzliConfig env0 gw
  mapM_ initialResizeImage resizesConfig

  where initialResizeImage :: ResizeConfig -> WorkerT IO ()
        initialResizeImage conf = addFunc (FuncName $ funcName conf) $ resizeImage conf env0 gw
        funcName = pack . imageFuncName
