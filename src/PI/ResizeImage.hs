{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PI.ResizeImage
  (
    resizeImage
  , ResizeConfig (..)
  ) where

import           Data.Aeson                (FromJSON, parseJSON, withObject,
                                            (.!=), (.:), (.:?))

import           Data.ByteString.Char8     (ByteString)
import qualified Data.ByteString.Lazy      as LB (toStrict)
import           Data.Int                  (Int64)
import           PI.Utils

import           Periodic.Client           (Client, submitJob)
import           Periodic.Job              (Job, name, workDone)

import           Graphics.Image            (Border (Edge), Nearest (Nearest),
                                            dims)

import qualified Control.Monad             as M (foldM)
import           Graphics.Image.Types

import           Graphics.Image.Processing (resize)
import           ShareFS.Client            (Gateway)

import           System.FilePath           (takeBaseName, (</>))
import           System.Log.Logger         (errorM)

data ResizeConfig = ResizeConfig { imageWidth    :: Int
                                 , imageOutput   :: FilePath
                                 , imageFuncName :: String
                                 , imageSuffix   :: String
                                 , imageDelay    :: Int64
                                 }
  deriving (Show)

instance FromJSON ResizeConfig where
  parseJSON = withObject "ResizeConfig" $ \o -> do
    imageWidth    <- o .:? "width"  .!= 100
    imageFuncName <- o .: "func"
    imageOutput   <- o .:? "output" .!= "resize-image"
    imageSuffix   <- o .:? "suffix" .!= "_fw100.jpg"
    imageDelay    <- o .:? "delay"  .!= 432000
    return ResizeConfig {..}

readImage :: ByteString -> IO (Either String (Image VS RGB Double))
readImage bs = M.foldM reader (Left "") formats

  where formats = [InputBMP, InputGIF, InputHDR, InputJPG, InputPNG, InputTIF, InputPNM, InputTGA]

        reader :: Either String (Image VS RGB Double) -> InputFormat -> IO (Either String (Image VS RGB Double))
        reader (Left err) format =
          return $ either (Left . ((err++"\n")++)) Right (decode format bs)
        reader img         _     = return img


resizeImage :: ResizeConfig -> Client -> Gateway -> Job -> IO ()
resizeImage (ResizeConfig {..}) c gw job = do
  getFileAndNext gw job $ \bs -> do
    decoded <- readImage $ LB.toStrict bs
    case decoded of
      Left err    -> errorM "ResizeImage" err >> workDone job
      Right img -> do
        let out = encode OutputJPG [] $ resize Nearest Edge (height (dims img), imageWidth) img
        putFileAndNext gw job outFileName out $ do
          submitJob c "upload" outFileName' 0
          submitJob c "remove" outFileName' imageDelay
          workDone job

  where outFileName = imageOutput </> takeBaseName (unpackBS $ name job) ++ imageSuffix
        outFileName' = packBS outFileName

        height :: (Int, Int) -> Int
        height (h, w) = imageWidth * h `div` w
