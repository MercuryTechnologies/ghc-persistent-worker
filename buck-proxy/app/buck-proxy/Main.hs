module Main where

import BuckProxy.Run (parseCliArgs, run)
import BuckProxy.Util (dbg)
import Control.Concurrent.MVar (MVar, newMVar, readMVar)
import Control.Exception (Exception (..), SomeException (..), try)
import Control.Monad (join, void)
import System.Exit (exitSuccess)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import System.Posix.Signals (Handler (Catch), installHandler, sigTERM)
import Types.Orchestration (envServerSocket)

onSigTERM :: MVar (IO ()) -> IO ()
onSigTERM refHandler = do
  dbg "buck-proxy: SigTERM is being handled..."
  join (readMVar refHandler)
  exitSuccess

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  options <- parseCliArgs
  socket <- envServerSocket
  refHandler <- newMVar (pure ())
  void $ installHandler sigTERM (Catch $ onSigTERM refHandler) Nothing
  try (run socket options refHandler) >>= \case
    Right () ->
      dbg "Worker terminated without cancellation."
    Left (err :: SomeException) ->
      dbg ("Worker terminated with exception: " ++ displayException err)
