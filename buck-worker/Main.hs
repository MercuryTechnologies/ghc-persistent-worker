{-# language DataKinds, OverloadedLists, OverloadedStrings, GADTs #-}

module Main where

import Args (parseBuckArgs, writeResult)
import Cache (Cache (..), emptyCache)
import Compile (compile)
import Control.Concurrent.MVar (MVar, newMVar, readMVar)
import Control.Exception (SomeException (SomeException), throwIO, try)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
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
import Worker (
  ExecuteCommand (..),
  ExecuteCommand_EnvironmentEntry (..),
  ExecuteEvent (..),
  ExecuteResponse (..),
  Worker (..),
  workerServer,
  )

commandEnv :: Vector.Vector ExecuteCommand_EnvironmentEntry -> Map String String
commandEnv =
  Map.fromList .
  fmap (\ (ExecuteCommand_EnvironmentEntry key value) -> (fromBs key, fromBs value)) .
  Vector.toList
  where
    fromBs = Text.unpack . decodeUtf8Lenient

executeHandler ::
  MVar Cache ->
  ServerRequest 'Normal ExecuteCommand ExecuteResponse ->
  IO (ServerResponse 'Normal ExecuteResponse)
executeHandler cache (ServerNormalRequest _ ExecuteCommand {executeCommandArgv, executeCommandEnv}) = do
  -- hPutStrLn stderr (unlines argv)
  response <- either exceptionResponse successResponse =<< try run
  pure (ServerNormalResponse response [] StatusOk "")
  where
    run = do
      args <- either (throwIO . userError) pure (parseBuckArgs (commandEnv executeCommandEnv) argv)
      log <- newMVar Log {diagnostics = [], other = [], debug = False}
      let env = Env {log, cache, args}
      result <- withGhc env (compile args)
      pure (env, result)

    successResponse (env, result) = do
      executeResponseExitCode <- writeResult env.args result
      Log {diagnostics, other} <- readMVar env.log
      pure ExecuteResponse {
        executeResponseExitCode,
        executeResponseStderr = LazyText.unlines (LazyText.pack <$> reverse (other ++ diagnostics))
      }

    exceptionResponse (SomeException e) =
      pure ExecuteResponse {
        executeResponseExitCode = 1,
        executeResponseStderr = "Uncaught exception: " <> LazyText.pack (show e)
      }

    argv = Text.unpack . decodeUtf8Lenient <$> Vector.toList executeCommandArgv

execHandler ::
  ServerRequest 'ClientStreaming ExecuteEvent ExecuteResponse ->
  IO (ServerResponse 'ClientStreaming ExecuteResponse)
execHandler (ServerReaderRequest _metadata _recv) = do
  hPutStrLn stderr "Received Exec"
  error "not implemented"

handlers :: MVar Cache -> Worker ServerRequest ServerResponse
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
  cache <- emptyCache True
  workerServer (handlers cache) (maybe id setSocket socket defaultServiceOptions)
  where
    setSocket s options = options {serverHost = fromString ("unix://" <> s <> "\x00"), serverPort = 0}
