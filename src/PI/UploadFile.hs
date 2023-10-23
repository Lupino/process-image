{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

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
import qualified Data.ByteString.Char8  as B (length, pack)
import           Foreign.C.Types
import qualified Language.C.Inline      as C
import           Periodic.Job           (JobM, count, name, schedLater',
                                         submitJob, workDone)
import           System.FilePath        ((</>))
import           System.Log.Logger      (errorM)

C.context (C.baseCtx <> C.bsCtx)

C.include "<stdio.h>"
C.include "<libbucket.h>"


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
  [C.block| int {
    GoString remotePath;
    remotePath.p = $bs-ptr:remotePath;
    remotePath.n = $(int len1);
    GoString fileName;
    fileName.p = $bs-ptr:fileName;
    fileName.n = $(int len2);
    return (int)GoUpload(remotePath, fileName);
  }|]

  where len1 = fromIntegral $ B.length remotePath
        len2 = fromIntegral $ B.length fileName

initBucket_ :: ByteString -> ByteString -> ByteString -> ByteString -> IO CInt
initBucket_ accessID accessKey bucketName endpoint =
  [C.block| int {
    GoString accessID;
    accessID.p = $bs-ptr:accessID;
    accessID.n = $(int len1);
    GoString accessKey;
    accessKey.p = $bs-ptr:accessKey;
    accessKey.n = $(int len2);
    GoString bucketName;
    bucketName.p = $bs-ptr:bucketName;
    bucketName.n = $(int len3);
    GoString endpoint;
    endpoint.p = $bs-ptr:endpoint;
    endpoint.n = $(int len4);
    return (int)GoInitBucket(accessID, accessKey, bucketName, endpoint);
  }|]
  where len1 = fromIntegral $ B.length accessID
        len2 = fromIntegral $ B.length accessKey
        len3 = fromIntegral $ B.length bucketName
        len4 = fromIntegral $ B.length endpoint

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
