module PI.Utils
  (
    doJobLater
  , getFileAndNext
  , putFileAndNext
  ) where

import qualified Data.ByteString.Lazy as LB (ByteString)
import           Periodic.Job         (Job, counter, name, schedLater',
                                       workDone)
import           ShareFS.Client       (Gateway, getFile, putFile)
import           System.Log.Logger    (errorM)

doJobLater :: Job -> String -> IO ()
doJobLater job err = do
  errorM "PI.Utils" err
  if counter job > 15 then workDone job
                      else schedLater' job (counter job * (10 + counter job)) 1

getFileAndNext :: Gateway -> Job -> (LB.ByteString -> IO ()) -> IO ()
getFileAndNext gw job next = do
  ret <- getFile (name job) gw
  case ret of
    Left err -> doJobLater job err
    Right bs -> next bs

putFileAndNext :: Gateway -> Job -> FilePath -> LB.ByteString -> IO () -> IO ()
putFileAndNext gw job fn bs next = do
  ret <- putFile fn bs gw
  case ret of
    Left err -> doJobLater job err
    Right _  -> next
