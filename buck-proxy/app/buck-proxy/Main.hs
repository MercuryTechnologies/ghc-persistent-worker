module Main where

import Control.Exception (Exception (..), SomeException (..), try)
import Control.Monad.IO.Class (MonadIO(liftIO))
import BuckProxy.Run (parseOptions, run)
import System.Environment (getArgs)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)
import Types.Orchestration (envServerSocket)

dbg :: MonadIO m => String -> m ()
dbg = liftIO . hPutStrLn stderr

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  options <- parseOptions =<< getArgs
  socket <- envServerSocket
  try (run socket options) >>= \case
    Right () ->
      dbg "Worker terminated without cancellation."
    Left (err :: SomeException) ->
      dbg ("Worker terminated with exception: " ++ displayException err)
