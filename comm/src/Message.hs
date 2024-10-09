{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Message
  ( Msg (..),
    sendMsg,
    recvMsg,
    wrapMsg,
    unwrapMsg,
    --
    Id (..),
    Request (..),
    Response (..),
  ) where

import Control.DeepSeq (deepseq)
import Control.Monad (replicateM)
import Data.Binary (Binary (..), encode, decode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Foldable (traverse_)
import Data.Int (Int32)
import GHC.Generics (Generic)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)

data Msg = Msg
  { msgNumBytes :: !Int32,
    msgPayload :: !ByteString
  }

chunkSize :: Int
chunkSize = 1024

toChunks :: Int -> ByteString -> [ByteString]
toChunks !bytes !bs0 = go id bs0 []
  where
    go !acc !bs =
      let (!chunk, !bs') = C.splitAt bytes bs
       in if C.length bs' < bytes
          then if C.null bs'
               then acc . (chunk:)
               else (acc . (chunk:) . (bs':))
          else go (acc . (chunk:)) bs'

sendMsg :: Socket -> Msg -> IO ()
sendMsg s (Msg !n !payload) = do
  sendAll s (L.toStrict (encode n))
  let !chunks = toChunks chunkSize payload
  chunks `deepseq` traverse_ (\chunk -> chunk `deepseq` sendAll s chunk) chunks

recvMsg :: Socket -> IO Msg
recvMsg s = do
  !msg_n <- recv s 4
  let n :: Int32
      n = decode (L.fromStrict msg_n)
      n' :: Int
      n' = fromIntegral n
      (q, r) = n' `divMod` chunkSize
  ps <- replicateM q (recv s chunkSize)
  payload <-
    if r > 0
    then do
      p <- recv s r
      pure $! mconcat (ps ++ [p])
    else
      pure $! mconcat ps
  payload `deepseq` pure (Msg n payload)

wrapMsg :: (Binary a) => a -> Msg
wrapMsg x =
  let bs = L.toStrict (encode x)
      n = fromIntegral $ C.length bs
   in Msg n bs

unwrapMsg :: (Binary a) => Msg -> a
unwrapMsg (Msg _n bs) = decode (L.fromStrict bs)

newtype Id = Id String
  deriving (Show, Eq, Binary)

data Request = Request
  { requestWorkerId :: Id,
    requestEnv :: [(String, String)],
    requestArgs :: [String]
  }
  deriving (Show, Generic)

instance Binary Request

data Response = Response
  { responseResult :: [String],
    responseConsoleStdOut :: [String],
    responseConsoleStdErr :: [String]
  }
  deriving (Show, Generic)

instance Binary Response

{-
newtype ConsoleOutput = ConsoleOutput
  { unConsoleOutput :: [String]
  }
  deriving Binary
-}
