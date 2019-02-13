module PI.Utils
  ( doJobLater
  , unpackBS
  , packBS
  ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as B (ByteString)
import qualified Data.ByteString.Lazy   as LB (ByteString, readFile)
import           Data.Text              as T (pack, unpack)
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Periodic.Job           (JobT, count, name, schedLater',
                                         workDone)
import           System.FilePath        ((</>))
import           System.Log.Logger      (errorM)

unpackBS :: B.ByteString -> String
unpackBS = T.unpack . decodeUtf8

packBS :: String -> B.ByteString
packBS = encodeUtf8 . T.pack

doJobLater :: String -> JobT IO ()
doJobLater err = do
  liftIO $ errorM "PI.Utils" err
  c <- count
  if c > 15 then workDone
            else schedLater' (fromIntegral $ later c) 1

  where later :: Int -> Int
        later c = c * (10 + c)
