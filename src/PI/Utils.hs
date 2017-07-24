module PI.Utils
  (
    doJobLater
  , getFileAndNext
  , putFileAndNext
  , unpackBS
  , packBS
  ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as B (ByteString, unpack)
import qualified Data.ByteString.Lazy   as LB (ByteString)
import           Data.Text              as T (pack, unpack)
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Periodic.Job           (Job, counter, name, schedLater',
                                         workDone)
import           ShareFS.Client         (Gateway, getFile, putFile)
import           System.Log.Logger      (errorM)

unpackBS :: B.ByteString -> String
unpackBS = T.unpack . decodeUtf8

packBS :: String -> B.ByteString
packBS = encodeUtf8 . T.pack

doJobLater :: String -> Job ()
doJobLater err = do
  liftIO $ errorM "PI.Utils" err
  c <- counter
  if c > 15 then workDone
            else schedLater' (fromIntegral $ later c) 1

  where later :: Int -> Int
        later c = c * (10 + c)

getFileAndNext :: Gateway -> (LB.ByteString -> Job ()) -> Job ()
getFileAndNext gw next = do
  n <- unpackBS <$> name
  ret <- liftIO $ getFile n gw
  case ret of
    Left err -> doJobLater err
    Right bs -> next bs

putFileAndNext :: Gateway -> FilePath -> LB.ByteString -> Job () -> Job ()
putFileAndNext gw fn bs next = do
  ret <- liftIO $ putFile fn bs gw
  case ret of
    Left err -> doJobLater err
    Right _  -> next
