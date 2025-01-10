{-# language DataKinds, GADTs, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import BuckArgs (BuckArgs (..), CompileResult (..), parseBuckArgs, toGhcArgs, writeResult)
import Control.Concurrent.MVar (MVar)
import Control.Exception (SomeException (SomeException), throwIO, try)
import Control.Monad (when)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import GHC (Ghc, getSession)
import Internal.AbiHash (AbiHash (..), showAbiHash)
import Internal.Cache (Cache (..), ModuleArtifacts (..), Target, emptyCache)
import Internal.Compile (compile)
import Internal.Log (logFlush, newLog)
import Internal.Session (Env (..), withGhc)
import Network.GRPC.Common (
  def,
  NextElem,
  NoMetadata,
  RequestMetadata,
  ResponseInitialMetadata,
  ResponseTrailingMetadata,
  defaultInsecurePort,
  )
import Network.GRPC.Common.Protobuf (Proto, Protobuf, defMessage, (&), (.~))
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.Run (InsecureConfig (InsecureConfig), ServerConfig (..), runServerWithHandlers)
import Network.GRPC.Server.StreamType (Methods (Method, NoMoreMethods), fromMethods, mkClientStreaming, mkNonStreaming)
import Prelude hiding (log)
import Proto.Worker (ExecuteCommand, ExecuteCommand'EnvironmentEntry, ExecuteEvent, ExecuteResponse, Worker (..))
import qualified Proto.Worker_Fields as Fields
import System.Environment (lookupEnv)
import System.IO (BufferMode (LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)

compileAndReadAbiHash :: BuckArgs -> Target -> Ghc (Maybe CompileResult)
compileAndReadAbiHash args target = do
  compile target >>= traverse \ artifacts -> do
    hsc_env <- getSession
    let
      abiHash :: Maybe AbiHash
      abiHash = do
        path <- args.abiOut
        Just AbiHash {path, hash = showAbiHash hsc_env artifacts.iface}
    pure CompileResult {artifacts, abiHash}

type instance RequestMetadata (Protobuf Worker "exec") = NoMetadata
type instance ResponseInitialMetadata (Protobuf Worker "exec") = NoMetadata
type instance ResponseTrailingMetadata (Protobuf Worker "exec") = NoMetadata

type instance RequestMetadata (Protobuf Worker "execute") = NoMetadata
type instance ResponseInitialMetadata (Protobuf Worker "execute") = NoMetadata
type instance ResponseTrailingMetadata (Protobuf Worker "execute") = NoMetadata

commandEnv :: [Proto ExecuteCommand'EnvironmentEntry] -> Map String String
commandEnv =
  Map.fromList .
  fmap \ kv -> (fromBs kv.key, fromBs kv.value)
  where
    fromBs = Text.unpack . decodeUtf8Lenient

execute ::
  MVar Cache ->
  Proto ExecuteCommand ->
  IO (Proto ExecuteResponse)
execute cache req = do
  when False do
    hPutStrLn stderr (unlines argv)
  either exceptionResponse successResponse =<< try run
  where
    run = do
      buckArgs <- either (throwIO . userError) pure (parseBuckArgs (commandEnv req.env) argv)
      args <- toGhcArgs buckArgs
      log <- newLog False
      let env = Env {log, cache, args}
      result <- withGhc env (compileAndReadAbiHash buckArgs)
      pure (env, buckArgs, result)

    successResponse (env, buckArgs, result) = do
      exitCode <- writeResult buckArgs result
      output <- logFlush env.log
      pure $ defMessage
        & Fields.exitCode .~ exitCode
        & Fields.stderr .~ Text.unlines (Text.pack <$> output)

    exceptionResponse (SomeException e) =
      pure $ defMessage
        & Fields.exitCode .~ 1
        & Fields.stderr .~ "Uncaught exception: " <> Text.pack (show e)

    argv = Text.unpack . decodeUtf8Lenient <$> req.argv

exec :: IO (NextElem (Proto ExecuteEvent)) -> IO (Proto ExecuteResponse)
exec _ =
  pure $ defMessage
    & Fields.exitCode .~ 1
    & Fields.stderr .~ "Streaming not implemented"

methods ::
  MVar Cache ->
  Methods IO (ProtobufMethodsOf Worker)
methods cache = Method (mkClientStreaming exec) (Method (mkNonStreaming (execute cache)) NoMoreMethods)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  socket <- lookupEnv "WORKER_SOCKET"
  hPutStrLn stderr $ "using worker socket: " <> show socket
  cache <- emptyCache True
  runServerWithHandlers def config $ fromMethods (methods cache)
  where
    config :: ServerConfig
    config =
      ServerConfig {
        serverInsecure = Just (InsecureConfig Nothing defaultInsecurePort),
        serverSecure = Nothing
      }

  -- workerServer (handlers cache) (maybe id setSocket socket defaultServiceOptions)
  -- where
  --   setSocket s options = options {serverHost = fromString ("unix://" <> s <> "\x00"), serverPort = 0}
