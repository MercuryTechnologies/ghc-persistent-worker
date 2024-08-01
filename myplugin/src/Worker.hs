{-# LANGUAGE NumericUnderscores #-}
module Worker where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Message (Msg (..), recvMsg, sendMsg, unwrapMsg, wrapMsg)
import Network.Socket (Socket)
import System.Environment (getArgs)
import System.IO
  ( BufferMode (..),
    IOMode (..),
    hFlush,
    hGetLine,
    hPutStrLn,
    hSetBuffering,
    stderr,
    stdout,
    withFile,
  )
import System.Posix.IO
  ( OpenFileFlags (nonBlock),
    OpenMode (ReadOnly, WriteOnly),
    defaultFileFlags,
    fdToHandle,
    openFd,
  )
import Util (openFileAfterCheck, openPipeRead, openPipeWrite)

logMessage :: String -> IO ()
logMessage = hPutStrLn stderr

workerMain :: [String] -> [String] -> IO ()
workerMain flags args = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  -- args <- getArgs
  let n :: Int = read (flags !! 0)
      infile = flags !! 1
      outfile = flags !! 2
      prompt = "[Worker:" ++ show n ++ "]"
  hin <- openFileAfterCheck infile (True, False) openPipeRead
  hout <- openFileAfterCheck outfile (False, True) openPipeWrite
  logMessage (prompt ++ " Started")
  forever $ do
    s <- hGetLine hin
    logMessage (prompt ++ " Got args: " ++ show s)
    threadDelay 15_000_000
    hPutStrLn hout "ABCDE"
    hFlush hout
