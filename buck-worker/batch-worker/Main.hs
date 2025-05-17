module Main where

import Control.Exception (Exception (..), SomeException (..), try)
import Internal.Log (dbg)
import Run (batchCompile)
import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  paths <- getArgs
  batchCompile paths
