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

import Control.Monad (replicateM)
import Data.Binary (Binary (..), encode, decode)
import Data.ByteString (ByteString (..))
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Foldable (traverse_)
import Data.Int (Int32 (..))
import Network.Socket (Socket (..))
import Network.Socket.ByteString (recv, sendAll)

data Msg = Msg
  { msgNumBytes :: Int32,
    msgPayload :: ByteString
  }

chunkSize :: Int
chunkSize = 4096

toChunks :: Int -> ByteString -> [ByteString]
toChunks bytes bs0 = go id bs0 []
  where
    go !acc !bs =
      let (chunk, bs') = C.splitAt bytes bs
       in if C.length bs' < bytes
          then (acc . (chunk:) . (bs':))
          else go (acc . (chunk:)) bs'

sendMsg :: Socket -> Msg -> IO ()
sendMsg s (Msg n payload) = do
  sendAll s (L.toStrict (encode n))
  let chunks = toChunks chunkSize payload
  -- putStrLn $ "sendMsg: " ++ show n ++ " bytes " ++ show (length chunks)
  traverse_ (sendAll s) chunks

recvMsg :: Socket -> IO Msg
recvMsg s = do
  msg_n <- recv s 4
  let n :: Int32
      n = decode (L.fromStrict msg_n)
      n' :: Int
      n' = fromIntegral n
      (q, r) = n' `divMod` chunkSize
  -- putStrLn $ "recvMsg: " ++ show n' ++ " bytes " ++ show (q, r)
  ps <- replicateM q (recv s chunkSize)
  p <- recv s r
  let payload = mconcat (ps ++ [p])
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
