module Main where

import Control.Concurrent.MVar (MVar, newMVar, readMVar)
import Control.Exception (Exception (..), SomeException (..), try)
import Control.Monad (void)
import BuckProxy.Run (parseOptions, run)
import BuckProxy.Util (dbg)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import System.Posix.Signals (installHandler, Handler(Catch), sigTERM)
import Types.Orchestration (envServerSocket)

onSigTERM :: MVar (IO ()) -> IO ()
onSigTERM refHandler = do
  dbg "buck-proxy: SigTERM is being handled..."
  action <- readMVar refHandler
  action
  exitSuccess

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  options <- parseOptions =<< getArgs
  socket <- envServerSocket
  refHandler <- newMVar (pure ())
  void $ installHandler sigTERM (Catch $ onSigTERM refHandler) Nothing
  try (run socket options refHandler) >>= \case
    Right () ->
      dbg "Worker terminated without cancellation."
    Left (err :: SomeException) ->
      dbg ("Worker terminated with exception: " ++ displayException err)
