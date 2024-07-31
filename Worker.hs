{-# LANGUAGE NumericUnderscores #-}
-- placeholder worker. later replaced by GHC frontend plugin
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Message (Msg (..), recvMsg, sendMsg, unwrapMsg, wrapMsg)
import Network.Socket (Socket)
import System.Environment (getArgs)
import System.IO (BufferMode (..), hFlush, hGetLine, hPutStrLn, hSetBuffering, stderr, stdin, stdout)

logMessage :: String -> IO ()
logMessage = hPutStrLn stderr

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  args <- getArgs
  let n :: Int = read (args !! 0)
      prompt = "[Worker:" ++ show n ++ "]"
  logMessage (prompt ++ " Started")
  forever $ do
    s <- hGetLine stdin
    logMessage (prompt ++ " Got args: " ++ show s)
    threadDelay 15_000_000
    hPutStrLn stdout "ABCDE"
    hFlush stdout
