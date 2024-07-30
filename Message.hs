module Message
  ( Msg (..),
    sendMsg,
    recvMsg,
  ) where

import Data.Binary (Binary (..), encode, decode)
import Data.ByteString (ByteString (..))
import qualified Data.ByteString.Lazy as L
import Data.Int (Int32 (..))
import Network.Socket (Socket (..))
import Network.Socket.ByteString (recv, sendAll)

data Msg = Msg
  { msgNumBytes :: Int32,
    msgPayload :: ByteString
  }

sendMsg :: Socket -> Msg -> IO ()
sendMsg s (Msg n payload) = do
  sendAll s (L.toStrict (encode n))
  sendAll s payload

recvMsg :: Socket -> IO Msg
recvMsg s = do
  msg_n <- recv s 4
  let n = decode (L.fromStrict msg_n)
  payload <- recv s (fromIntegral n)
  pure (Msg n payload)
