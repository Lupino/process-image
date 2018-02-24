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

import           Periodic.Client           (Connection, runClient_, submitJob)
import           Periodic.Job              (Job, name, workDone)
import           Periodic.Monad            (unsafeLiftIO)
import           Periodic.Types            (JobName (..))

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


resizeImage :: ResizeConfig -> Connection -> Gateway -> Job ()
resizeImage ResizeConfig{..} c gw =
  getFileAndNext gw $ \bs -> do
    decoded <- unsafeLiftIO $ readImage $ LB.toStrict bs
    case decoded of
      Left err  -> unsafeLiftIO (errorM "ResizeImage" err) >> workDone
      Right img -> do
        n <- name
        let out = encode OutputJPG [] $ resize Nearest Edge (height (dims img), imageWidth) img
            outFileName = imageOutput </> takeBaseName n ++ imageSuffix
            outFileName' = JobName $ packBS outFileName
        putFileAndNext gw outFileName out $ do
          unsafeLiftIO $ runClient_ c $ do
            submitJob "upload-next-guetzli" outFileName' 0
            submitJob "remove" outFileName' imageDelay
          workDone

  where height :: (Int, Int) -> Int
        height (h, w) = imageWidth * h `div` w
