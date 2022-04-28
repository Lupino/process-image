{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PI.ResizeImage
  ( resizeImage
  , ResizeConfig (..)
  ) where

import           Codec.Picture          (DynamicImage (..), Image, readImage,
                                         saveJpgImage)
import qualified Codec.Picture          as P (Image (..))
import           Codec.Picture.Extra    (scaleBilinear)
import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON, parseJSON, withObject, (.!=),
                                         (.:), (.:?))
import           Data.String            (fromString)
import           Periodic.Job           (JobM, name, submitJob, workDone)
import           System.FilePath        (takeBaseName, (</>))
import           System.Log.Logger      (errorM)

data ResizeConfig = ResizeConfig { imageWidth    :: Int
                                 , imageOutput   :: FilePath
                                 , imageFuncName :: String
                                 , imageSuffix   :: String
                                 }
  deriving (Show)

instance FromJSON ResizeConfig where
  parseJSON = withObject "ResizeConfig" $ \o -> do
    imageWidth    <- o .:? "width"  .!= 100
    imageFuncName <- o .: "func"
    imageOutput   <- o .:? "output" .!= "resize-image"
    imageSuffix   <- o .:? "suffix" .!= "_fw100.jpg"
    return ResizeConfig {..}

resizeImage :: ResizeConfig -> FilePath -> JobM ()
resizeImage ResizeConfig{..} root = do
  fn <- name
  decoded <- liftIO $ readImage $ root </> fn
  case decoded of
    Left err  -> liftIO (errorM "PI.ResizeImage" err)
    Right img ->
      case scale img of
        Nothing -> liftIO (errorM "PI.ResizeImage" $ "Not support image " ++ fn)
        Just out -> do
          let outFileName = imageOutput </> takeBaseName fn ++ imageSuffix

          liftIO $ saveJpgImage 80 (root </> outFileName) out
          void $ submitJob "upload-next-guetzli" (fromString outFileName) "" 0 0
  workDone

  where height :: Image a -> Int
        height img = imageWidth * P.imageHeight img `div` P.imageWidth img

        scale :: DynamicImage -> Maybe DynamicImage
        scale (ImageY8 img)     = Just $ ImageY8 $ scale' img
        scale (ImageY16 img)    = Just $ ImageY16 $ scale' img
        scale (ImageY32 img)    = Just $ ImageY32 $ scale' img
        scale (ImageYA8 img)    = Just $ ImageYA8 $ scale' img
        scale (ImageYA16 img)   = Just $ ImageYA16 $ scale' img
        scale (ImageRGB8 img)   = Just $ ImageRGB8 $ scale' img
        scale (ImageRGB16 img)  = Just $ ImageRGB16 $ scale' img
        scale (ImageRGBA8 img)  = Just $ ImageRGBA8 $ scale' img
        scale (ImageRGBA16 img) = Just $ ImageRGBA16 $ scale' img
        scale (ImageYCbCr8 img) = Just $ ImageYCbCr8 $ scale' img
        scale (ImageCMYK8 img)  = Just $ ImageCMYK8 $ scale' img
        scale (ImageCMYK16 img) = Just $ ImageCMYK16 $ scale' img
        scale _                 = Nothing

        scale' img = scaleBilinear imageWidth (height img) img
