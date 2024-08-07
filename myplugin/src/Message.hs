{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Message
  ( Msg (..),
    sendMsg,
    recvMsg,
    wrapMsg,
    unwrapMsg,
    --
    ConsoleOutput (..),
  ) where

import Data.Binary (Binary (..), encode, decode)
import Data.ByteString (ByteString (..))
import qualified Data.ByteString.Char8 as C
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

wrapMsg :: (Binary a) => a -> Msg
wrapMsg x =
  let bs = L.toStrict (encode x)
      n = fromIntegral $ C.length bs
   in Msg n bs

unwrapMsg :: (Binary a) => Msg -> a
unwrapMsg (Msg _n bs) = decode (L.fromStrict bs)

newtype ConsoleOutput = ConsoleOutput
  { unConsoleOutput :: [String]
  }
  deriving Binary
