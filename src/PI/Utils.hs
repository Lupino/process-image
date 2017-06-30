module PI.Utils
  (
    doJobLater
  , getFileAndNext
  , putFileAndNext
  , unpackBS
  , packBS
  ) where

import qualified Data.ByteString.Char8 as B (ByteString, unpack)
import qualified Data.ByteString.Lazy  as LB (ByteString)
import           Data.Text             as T (pack, unpack)
import           Data.Text.Encoding    (decodeUtf8, encodeUtf8)
import           Periodic.Job          (Job, counter, name, schedLater',
                                        workDone)
import           ShareFS.Client        (Gateway, getFile, putFile)
import           System.Log.Logger     (errorM)

unpackBS :: B.ByteString -> String
unpackBS = T.unpack . decodeUtf8

packBS :: String -> B.ByteString
packBS = encodeUtf8 . T.pack

doJobLater :: Job -> String -> IO ()
doJobLater job err = do
  errorM "PI.Utils" err
  if counter job > 15 then workDone job
                      else schedLater' job (fromIntegral $ later) 1

  where later :: Int
        later = counter job * (10 + counter job)

getFileAndNext :: Gateway -> Job -> (LB.ByteString -> IO ()) -> IO ()
getFileAndNext gw job next = do
  ret <- getFile (unpackBS $ name job) gw
  case ret of
    Left err -> doJobLater job err
    Right bs -> next bs

putFileAndNext :: Gateway -> Job -> FilePath -> LB.ByteString -> IO () -> IO ()
putFileAndNext gw job fn bs next = do
  ret <- putFile fn bs gw
  case ret of
    Left err -> doJobLater job err
    Right _  -> next
