{-# language DataKinds, OverloadedLists, OverloadedStrings, GADTs #-}

module Main where

import Args (parseBuckArgs, writeResult)
import Cache (CacheRef)
import Compile (compile)
import Control.Concurrent.MVar (newMVar, readMVar)
import Control.Exception (throwIO)
import Data.Foldable (traverse_)
import Data.IORef (newIORef)
import Data.String (fromString)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import qualified Data.Text.Lazy as LazyText
import qualified Data.Vector as Vector
import Log (Log (..))
import Network.GRPC.HighLevel.Generated (
  GRPCMethodType (..),
  ServerRequest (..),
  ServerResponse (..),
  ServiceOptions (..),
  StatusCode (..),
  defaultServiceOptions,
  )
import Prelude hiding (log)
import Session (Env (..), withGhc)
import System.Environment (lookupEnv)
import System.IO (BufferMode (LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)
import Worker (ExecuteCommand (..), ExecuteEvent (..), ExecuteResponse (..), Worker (..), workerServer)

executeHandler ::
  CacheRef ->
  ServerRequest 'Normal ExecuteCommand ExecuteResponse ->
  IO (ServerResponse 'Normal ExecuteResponse)
executeHandler cache (ServerNormalRequest _ ExecuteCommand {executeCommandArgv}) = do
  hPutStrLn stderr (unlines argv)
  args <- either (throwIO . userError) pure (parseBuckArgs argv)
  log <- newMVar Log {diagnostics = [], other = []}
  result <- withGhc Env {log, cache, args} (compile args)
  executeResponseExitCode <- writeResult args result
  Log {diagnostics, other} <- readMVar log
  traverse_ (hPutStrLn stderr) (reverse other)
  let
    response = ExecuteResponse {
      executeResponseExitCode,
      executeResponseStderr = mconcat (LazyText.pack <$> reverse diagnostics)
      }
  pure (ServerNormalResponse response [] StatusOk "")
  where
    argv = Text.unpack . decodeUtf8Lenient <$> Vector.toList executeCommandArgv

execHandler ::
  ServerRequest 'ClientStreaming ExecuteEvent ExecuteResponse ->
  IO (ServerResponse 'ClientStreaming ExecuteResponse)
execHandler (ServerReaderRequest _metadata _recv) = do
  hPutStrLn stderr "Received Exec"
  error "not implemented"

handlers :: CacheRef -> Worker ServerRequest ServerResponse
handlers cache =
  Worker
    { workerExecute = executeHandler cache,
      workerExec = execHandler
    }

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  socket <- lookupEnv "WORKER_SOCKET"
  hPutStrLn stderr $ "using worker socket: " <> show socket
  cache <- newIORef Nothing
  workerServer (handlers cache) (maybe id setSocket socket defaultServiceOptions)
  where
    setSocket s options = options {serverHost = fromString ("unix://" <> s <> "\x00"), serverPort = 0}
