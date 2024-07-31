{-# LANGUAGE NumericUnderscores #-}
-- placeholder worker. later replaced by GHC frontend plugin
module Main where

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
    -- stdin,
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
import Util (fileOpenAfterCheck)

logMessage :: String -> IO ()
logMessage = hPutStrLn stderr

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  args <- getArgs
  let n :: Int = read (args !! 0)
      infile = args !! 1
      outfile = args !! 2
      prompt = "[Worker:" ++ show n ++ "]"
  hin <-
    fileOpenAfterCheck infile (True, False) $ \fp -> do
      fd <- openFd fp ReadOnly defaultFileFlags {nonBlock = True}
      h <- fdToHandle fd
      pure h
  hout <-
    fileOpenAfterCheck outfile (False, True) $ \fp -> do
      fd <- openFd fp WriteOnly defaultFileFlags {nonBlock = True}
      h <- fdToHandle fd
      pure h
  logMessage (prompt ++ " Started")
  forever $ do
    s <- hGetLine hin
    logMessage (prompt ++ " Got args: " ++ show s)
    threadDelay 15_000_000
    hPutStrLn hout "ABCDE"
    hFlush hout
