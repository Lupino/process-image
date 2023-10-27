{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PI.GuetzliImage
  ( GuetzliConfig (..)
  , guetzliImage
  , defaultGuetzliConfig
  ) where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON, parseJSON, withObject, (.!=),
                                         (.:?))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as B (length, pack, useAsCString)
import           Data.String            (fromString)
import           Foreign.C.Types
import           Foreign.C.String (CString)
import           Periodic.Job           (JobM, count, name, schedLater',
                                         submitJob, withLock, workDone)
import           Periodic.Types         (LockName (..))
import           System.FilePath        (takeFileName, (</>))
import           System.Log.Logger      (errorM)

#include "guetzli.h"

data GuetzliConfig = GuetzliConfig
  { guetzliOutput     :: FilePath
  , guetzliLName      :: Maybe String
  , guetzliLCount     :: Int
  , guetzliVerbose    :: Int
  , guetzliQuality    :: Int
  , guetzliMemlimitMb :: Int
  }
  deriving (Show)

instance FromJSON GuetzliConfig where
  parseJSON = withObject "GuetzliConfig" $ \o -> do
    guetzliOutput     <- o .:? "output"      .!= "guetzli"
    guetzliLName      <- o .:? "lock-name"
    guetzliLCount     <- o .:? "lock-count"  .!= 1
    guetzliVerbose    <- o .:? "verbose"     .!= 0
    guetzliQuality    <- o .:? "quality"     .!= 95
    guetzliMemlimitMb <- o .:? "memlimit_mb" .!= 6000
    return GuetzliConfig {..}

defaultGuetzliConfig :: GuetzliConfig
defaultGuetzliConfig = GuetzliConfig
  { guetzliOutput = "guetzli"
  , guetzliLName = Nothing
  , guetzliLCount = 1
  , guetzliVerbose = 0
  , guetzliQuality = 95
  , guetzliMemlimitMb = 6000
  }


foreign import ccall "guetzliMain" c_guetzliMain
  :: CInt -> CString -> CInt -> CString -> CInt -> CInt -> CInt -> IO CInt

guetzliMain :: ByteString -> ByteString -> Int -> Int -> Int -> IO CInt
guetzliMain infile outfile verbose quality memlimit_mb =
  B.useAsCString infile $ \infilePtr ->
  B.useAsCString outfile $ \outfilePtr ->
    c_guetzliMain (fromIntegral $ B.length infile) infilePtr (fromIntegral $ B.length outfile) outfilePtr (fromIntegral verbose) (fromIntegral quality) (fromIntegral memlimit_mb)

guetzliImage' :: GuetzliConfig -> FilePath -> JobM ()
guetzliImage' GuetzliConfig{..} root = do
  fn <- name
  let outFileName = guetzliOutput </> takeFileName fn
  code <- liftIO $ guetzliMain (B.pack $ root </> fn) (B.pack $ root </> outFileName) guetzliVerbose guetzliQuality guetzliMemlimitMb

  case code of
    0   -> do
      void $ submitJob "remove" (fromString fn) "" 300 0
      void $ submitJob "upload-next-remove" (fromString outFileName) "" 0 0

      void workDone
    _ -> do
      liftIO $ errorM "PI.GuetzliImage" $ "guetzli failed " ++ root </> fn
      c <- count
      if c > 15 then void workDone
                else void $ schedLater' (fromIntegral $ later c) 1


  where later :: Int -> Int
        later c = c * (10 + c)

guetzliImage :: GuetzliConfig -> FilePath -> JobM ()
guetzliImage c@GuetzliConfig{..} = f guetzliLName . guetzliImage' c
  where f :: Maybe String -> JobM () -> JobM ()
        f Nothing  = id
        f (Just n) = withLock (LockName $ fromString n) guetzliLCount
