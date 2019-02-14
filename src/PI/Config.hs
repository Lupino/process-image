{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PI.Config
  ( Config (..)
  ) where

import           Data.Aeson      (FromJSON, parseJSON, withObject, (.!=), (.:?))
import           PI.GuetzliImage (GuetzliConfig, defaultGuetzliConfig)
import           PI.ResizeImage  (ResizeConfig)

data Config = Config { periodicHost  :: String
                     , threadNum     :: Int
                     , root          :: FilePath
                     , guetzliConfig :: GuetzliConfig
                     , resizesConfig :: [ResizeConfig]
                     , enableRemove  :: Bool
                     , enableSave    :: Bool
                     , enableGuetzli :: Bool
                     }
  deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    periodicHost  <- o .:? "periodic"        .!= "unix:///tmp/periodic.sock"
    threadNum     <- o .:? "thread"          .!= 2
    root          <- o .:? "root"            .!= "images"
    guetzliConfig <- o .:? "guetzli"         .!= defaultGuetzliConfig
    resizesConfig <- o .:? "resizes"         .!= []
    enableRemove  <- o .:? "enable-remove"   .!= False
    enableSave    <- o .:? "enable-save"     .!= False
    enableGuetzli <- o .:? "enable-guetzli"  .!= False
    return Config {..}
