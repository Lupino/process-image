{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PI
  (
    module X
  , initialGateway
  , initialWorker
  ) where

import           PI.Config              as X
import           PI.GuetzliImage        as X
import           PI.RemoveFile          as X
import           PI.ResizeImage         as X

import           ShareFS.Client         (Gateway (..), initMgr)

import           Control.Monad.IO.Class (liftIO)
import           Periodic.Client        (Connection)
import           Periodic.Worker        (Worker, addFunc)

import           Control.Monad          (when)
import           Data.ByteString.Char8  (pack)

initialGateway :: Config -> IO Gateway
initialGateway Config{..} = initMgr Gateway
  { getGWUri       = shareFSHost
  , getGWAppKey    = shareFSKey
  , getGWAppSecret = shareFSSecret
  , getGWTimeout   = 1000
  , getGWConnCount = 100
  , getGWMgr       = Nothing
  }

initialWorker :: Connection -> Gateway -> Config -> Worker ()
initialWorker c gw Config{..} = do
  when enableRemove $ addFunc "remove" $ removeFile gw
  when enableGuetzli $ addFunc "guetzli" $ guetzliImage guetzliConfig c gw
  mapM_ initialResizeImage resizesConfig

  where initialResizeImage :: ResizeConfig -> Worker ()
        initialResizeImage conf = addFunc (funcName conf) $ resizeImage conf c gw
        funcName = pack . imageFuncName
