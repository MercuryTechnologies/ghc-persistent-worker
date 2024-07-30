{-# LANGUAGE NumericUnderscores #-}
-- placeholder worker. later replaced by GHC frontend plugin
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import System.Environment (getArgs)
import System.IO (BufferMode (..), hGetChar, hGetLine, hPutStrLn, hSetBuffering, stderr, stdin)

logMessage :: String -> IO ()
logMessage = hPutStrLn stderr

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  args <- getArgs
  let n :: Int = read (args !! 0)
      prompt = "[Worker:" ++ show n ++ "]"
  logMessage (prompt ++ " Started")
  forever $ do
    s <- hGetChar stdin
    logMessage (prompt ++ " Got args: " ++ show s)
  -- threadDelay 100_000_000
