module Main where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import Data.Binary (decode)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Int (Int32)
import Message (Msg (..), recvMsg)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = runServer "/tmp/mytest.ipc" talk
  where
    talk s = do
        Msg _ payload <- recvMsg s
        putStrLn "message received"
        unless (S.null payload) $ do
          sendAll s payload
          talk s

runServer :: FilePath -> (Socket -> IO a) -> IO a
runServer fp server = withSocketsDo $ E.bracket (open fp) close loop
  where
    open fp = E.bracketOnError (socket AF_UNIX Stream 0) close $ \sock -> do
        bind sock (SockAddrUnix fp)
        listen sock 5
        return sock
    loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (server conn) (const $ gracefulClose conn 5000)
