module PI.Utils
  (
    doJobLater
  , getFileAndNext
  , putFileAndNext
  , unpackBS
  , packBS
  ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as B (ByteString)
import qualified Data.ByteString.Lazy   as LB (ByteString)
import           Data.Text              as T (pack, unpack)
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Periodic.Job           (JobT, counter, name, schedLater',
                                         workDone)
import           ShareFS.Client         (Gateway, getFile, putFile)
import           System.Log.Logger      (errorM)

unpackBS :: B.ByteString -> String
unpackBS = T.unpack . decodeUtf8

packBS :: String -> B.ByteString
packBS = encodeUtf8 . T.pack

doJobLater :: String -> JobT IO ()
doJobLater err = do
  liftIO $ errorM "PI.Utils" err
  c <- counter
  if c > 15 then workDone
            else schedLater' (fromIntegral $ later c) 1

  where later :: Int -> Int
        later c = c * (10 + c)

getFileAndNext :: Gateway -> (LB.ByteString -> JobT IO ()) -> JobT IO ()
getFileAndNext gw next = do
  n <- name
  ret <- liftIO $ getFile n gw
  case ret of
    Left err -> doJobLater err
    Right bs -> next bs

putFileAndNext :: Gateway -> FilePath -> LB.ByteString -> JobT IO () -> JobT IO ()
putFileAndNext gw fn bs next = do
  ret <- liftIO $ putFile fn bs gw
  case ret of
    Left err -> doJobLater err
    Right _  -> next
