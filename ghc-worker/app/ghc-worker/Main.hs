module Main where

import Control.Exception (Exception (..), SomeException (..), try)
import Control.Monad.IO.Class (MonadIO(liftIO))
import GhcWorker.Run (parseOptions, runWorker)
import System.Environment (getArgs)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)

dbg :: MonadIO m => String -> m ()
dbg = liftIO . hPutStrLn stderr

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  options <- parseOptions =<< getArgs
  try (runWorker options) >>= \case
    Right () ->
      dbg "Worker terminated without cancellation."
    Left (err :: SomeException) ->
      dbg ("Worker terminated with exception: " ++ displayException err)
