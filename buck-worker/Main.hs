{-# language DataKinds, OverloadedLists, OverloadedStrings, GADTs #-}

module Main where

import Args (BuckArgs (..), CompileResult (..), parseBuckArgs, toGhcArgs, writeResult)
import BuckWorker (
  ExecuteCommand (..),
  ExecuteCommand_EnvironmentEntry (..),
  ExecuteEvent (..),
  ExecuteResponse (..),
  Worker (..),
  workerServer,
  )
import Control.Concurrent.MVar (MVar, newMVar, readMVar)
import Control.Exception (SomeException (SomeException), throwIO, try)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import qualified Data.Text.Lazy as LazyText
import qualified Data.Vector as Vector
import GHC (Ghc, Phase, getSession)
import Internal.AbiHash (readAbiHash)
import Internal.Cache (Cache (..), emptyCache)
import Internal.Compile (compile)
import Internal.Log (Log (..), newLog)
import Internal.Session (Env (..), withGhc)
import Network.GRPC.HighLevel.Generated (
  GRPCMethodType (..),
  ServerRequest (..),
  ServerResponse (..),
  ServiceOptions (..),
  StatusCode (..),
  defaultServiceOptions,
  )
import Prelude hiding (log)
import System.Environment (lookupEnv)
import System.IO (BufferMode (LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)

commandEnv :: Vector.Vector ExecuteCommand_EnvironmentEntry -> Map String String
commandEnv =
  Map.fromList .
  fmap (\ (ExecuteCommand_EnvironmentEntry key value) -> (fromBs key, fromBs value)) .
  Vector.toList
  where
    fromBs = Text.unpack . decodeUtf8Lenient

compileAndReadAbiHash :: BuckArgs -> [(String, Maybe Phase)] -> Ghc (Maybe CompileResult)
compileAndReadAbiHash args srcs = do
  compile srcs
  hsc_env <- getSession
  abiHash <- readAbiHash hsc_env args.abiOut
  pure (Just CompileResult {abiHash})

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
      buckArgs <- either (throwIO . userError) pure (parseBuckArgs (commandEnv executeCommandEnv) argv)
      args <- toGhcArgs buckArgs
      log <- newLog False
      let env = Env {log, cache, args}
      result <- withGhc env (compileAndReadAbiHash buckArgs)
      pure (env, buckArgs, result)

    successResponse (env, buckArgs, result) = do
      executeResponseExitCode <- writeResult buckArgs result
      output <- logFlush env.log
      pure ExecuteResponse {
        executeResponseExitCode,
        executeResponseStderr = LazyText.unlines (LazyText.pack <$> output)
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
