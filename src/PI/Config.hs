{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PI.Config
  (
    Config (..)
  )where

import           Data.Aeson      (FromJSON, parseJSON, withObject, (.!=), (.:?))
import           Data.Int        (Int64)

import           PI.GuetzliImage (GuetzliConfig, defaultGuetzliConfig)
import           PI.ResizeImage  (ResizeConfig)

data Config = Config { periodicHost  :: String
                     , threadNum     :: Int
                     , shareFSHost   :: String
                     , shareFSKey    :: String
                     , shareFSSecret :: String
                     , guetzliConfig :: GuetzliConfig
                     , resizesConfig :: [ResizeConfig]
                     , enableRemove  :: Bool
                     , enableGuetzli :: Bool
                     }
  deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    periodicHost  <- o .:? "periodic"        .!= "unix:///tmp/periodic.sock"
    threadNum     <- o .:? "thread"          .!= 2
    shareFSHost   <- o .:? "share-fs-host"   .!= "http://gw.huabot.com"
    shareFSKey    <- o .:? "share-fs-key"    .!= ""
    shareFSSecret <- o .:? "share-fs-secret" .!= ""
    guetzliConfig <- o .:? "guetzli"         .!= defaultGuetzliConfig
    resizesConfig <- o .:? "resizes"         .!= []
    enableRemove  <- o .:? "enable-remove"   .!= False
    enableGuetzli <- o .:? "enable-guetzli"  .!= False
    return Config {..}
