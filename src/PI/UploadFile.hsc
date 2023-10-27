{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PI.UploadFile
  ( BucketConfig (..)
  , initBucket
  , uploadImage
  , uploadNextGuetzli
  , uploadNextRemove
  ) where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON, parseJSON, withObject, (.!=),
                                         (.:), (.:?))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as B (pack, useAsCString)
import           Foreign.C.Types
import           Foreign.C.String       (CString)
import           Periodic.Job           (JobM, count, name, schedLater',
                                         submitJob, workDone)
import           System.FilePath        ((</>))
import           System.Log.Logger      (errorM)


#include "libbucket.h"


foreign import ccall
  "GoUpload" go_upload :: CString -> CString -> IO CInt

foreign import ccall
  "GoInitBucket" go_init_bucket :: CString -> CString -> CString -> CString -> IO CInt


data BucketConfig = BucketConfig
  { bucketAccessID  :: String
  , bucketAccessKey :: String
  , bucketName      :: String
  , bucketEndpoint  :: String
  , bucketPath      :: String
  }
  deriving (Show)

instance FromJSON BucketConfig where
  parseJSON = withObject "BucketConfig" $ \o -> do
    bucketAccessID  <- o .: "access_id"
    bucketAccessKey <- o .: "access_key"
    bucketName      <- o .: "name"
    bucketEndpoint  <- o .: "endpoint"
    bucketPath      <- o .:? "root_path" .!= "/"
    return BucketConfig {..}


upload :: ByteString -> ByteString -> IO CInt
upload remotePath fileName =
  B.useAsCString remotePath $ \remotePathPtr ->
  B.useAsCString fileName $ \fileNamePtr ->
    go_upload remotePathPtr fileNamePtr

initBucket_ :: ByteString -> ByteString -> ByteString -> ByteString -> IO CInt
initBucket_ accessID accessKey bucketName endpoint =
  B.useAsCString accessID $ \accessIDPtr ->
  B.useAsCString accessKey $ \accessKeyPtr ->
  B.useAsCString bucketName $ \bucketNamePtr ->
  B.useAsCString endpoint $ \endpointPtr ->
    go_init_bucket accessIDPtr accessKeyPtr bucketNamePtr endpointPtr

initBucket :: BucketConfig -> IO CInt
initBucket BucketConfig {..} = initBucket_ aid akey bn ep
  where aid  = B.pack bucketAccessID
        akey = B.pack bucketAccessKey
        bn   = B.pack bucketName
        ep   = B.pack bucketEndpoint


uploadImage_ :: String -> FilePath -> JobM () -> JobM ()
uploadImage_ remotePath root next = do
  fn <- name
  code <- liftIO $ upload (B.pack remotePath) (B.pack $ root </> fn)
  case code of
    0 -> void workDone >> next
    _ -> do
      liftIO $ errorM "PI.GuetzliImage" $ "guetzli failed " ++ root </> fn
      c <- count
      if c > 15 then void workDone >> next
                else void $ schedLater' (fromIntegral $ later c) 1


  where later :: Int -> Int
        later c = c * (10 + c)


uploadImage :: String -> FilePath -> JobM ()
uploadImage remotePath root = uploadImage_ remotePath root (return ())

uploadNextRemove :: String -> FilePath -> JobM ()
uploadNextRemove remotePath root =
  uploadImage_ remotePath root $ do
    fn <- name
    void $ submitJob "remove" fn "" 300 0


uploadNextGuetzli :: String -> FilePath -> JobM ()
uploadNextGuetzli remotePath root =
  uploadImage_ remotePath root $ do
    fn <- name
    void $ submitJob "guetzli" fn "" 0 0
