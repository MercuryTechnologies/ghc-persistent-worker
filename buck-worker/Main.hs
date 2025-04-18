module Main where

import Control.Exception (Exception (..), SomeException (..), try)
import Internal.Log (dbg)
import Orchestration (envServerSocket)
import Run (parseOptions, runWorker)
import System.Environment (getArgs)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  options <- parseOptions =<< getArgs
  socket <- envServerSocket
  hPutStrLn stderr $ "using worker socket: " <> show socket
  try (runWorker socket options) >>= \case
    Right () ->
      dbg "Worker terminated without cancellation."
    Left (err :: SomeException) ->
      dbg ("Worker terminated with exception: " ++ displayException err)
