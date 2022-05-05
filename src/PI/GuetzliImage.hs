{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

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
import qualified Data.ByteString.Char8  as B (length, pack)
import           Data.String            (fromString)
import           Foreign.C.Types
import qualified Language.C.Inline.Cpp  as C
import           Periodic.Job           (JobM, count, name, schedLater',
                                         submitJob, withLock, workDone)
import           Periodic.Types         (LockName (..))
import           System.FilePath        (takeFileName, (</>))
import           System.Log.Logger      (errorM)

C.context (C.cppCtx <> C.bsCtx)

C.include "<stdio.h>"
C.include "<guetzli.h>"

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

guetzliMain :: ByteString -> ByteString -> Int -> Int -> Int -> IO CInt
guetzliMain infile outfile verbose quality memlimit_mb =
  [C.exp| int {guetzliMain($(int len1), $bs-ptr:infile, $(int len2), $bs-ptr:outfile, $(int cverbose), $(int cquality), $(int cmemlimit_mb))}|]
  where len1         = fromIntegral $ B.length infile
        len2         = fromIntegral $ B.length outfile
        cverbose     = fromIntegral verbose
        cquality     = fromIntegral quality
        cmemlimit_mb = fromIntegral memlimit_mb


guetzliImage' :: GuetzliConfig -> FilePath -> JobM ()
guetzliImage' GuetzliConfig{..} root = do
  fn <- name
  let outFileName = guetzliOutput </> takeFileName fn
  code <- liftIO $ guetzliMain (B.pack $ root </> fn) (B.pack $ root </> outFileName) guetzliVerbose guetzliQuality guetzliMemlimitMb

  case code of
    0   -> do
      void $ submitJob "remove" (fromString fn) "" 300 0
      void $ submitJob "upload-next-remove" (fromString outFileName) "" 0 0

      workDone
    _ -> do
      liftIO $ errorM "PI.GuetzliImage" $ "guetzli failed " ++ root </> fn
      c <- count
      if c > 15 then workDone
                else schedLater' (fromIntegral $ later c) 1


  where later :: Int -> Int
        later c = c * (10 + c)

guetzliImage :: GuetzliConfig -> FilePath -> JobM ()
guetzliImage c@GuetzliConfig{..} = f guetzliLName . guetzliImage' c
  where f :: Maybe String -> JobM () -> JobM ()
        f Nothing  = id
        f (Just n) = withLock (LockName $ fromString n) guetzliLCount
