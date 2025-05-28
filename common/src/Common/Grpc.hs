module Common.Grpc (
  GrpcHandler (..),
  debugRequestArgs,
  commandEnv,
  execute,
  streamingNotImplemented,
  fromGrpcHandler,
) where

import BuckWorker (ExecuteCommand, ExecuteResponse)
import Control.Exception (SomeException (..), displayException, fromException, try)
import Control.Monad (when)
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import Network.GRPC.Common (NextElem (..))
import Network.GRPC.Common.Protobuf (Proto, defMessage, (&), (.~))
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (
  Methods (..),
  mkClientStreaming,
  mkNonStreaming,
  simpleMethods,
  )
import Proto.Worker (ExecuteCommand'EnvironmentEntry, ExecuteEvent, Worker)
import Proto.Worker_Fields qualified as Fields
import System.Exit (ExitCode (..), exitSuccess)
import System.IO (hPutStrLn, stderr)
import Types.Grpc (CommandEnv (..), RequestArgs (..))

debugRequestArgs :: Bool
debugRequestArgs = False

-- | A handler for gRPC requests takes a 'Map' of environment variables and a list of command line arguments and returns
-- a list of output message lines and an exit code.
newtype GrpcHandler =
  GrpcHandler {
    run ::
      CommandEnv ->
      RequestArgs ->
      IO ([String], Int32)
  }

commandEnv :: [Proto ExecuteCommand'EnvironmentEntry] -> CommandEnv
commandEnv =
  CommandEnv .
  Map.fromList .
  fmap \kv -> (fromBs kv.key, fromBs kv.value)
  where
    fromBs = Text.unpack . decodeUtf8Lenient

-- | Generic wrapper for a handler of the 'Worker' message 'ExecuteCommand', taking care of input data conversions and
-- response construction.
execute ::
  GrpcHandler ->
  Proto ExecuteCommand ->
  IO (Proto ExecuteResponse)
execute handler req = do
  when debugRequestArgs do
    hPutStrLn stderr (unlines argv)
  eres <- try (handler.run (commandEnv req.env) (RequestArgs argv))
  (output, exitCode) <-
    case eres of
      Right (output, exitCode) -> pure (output, exitCode)
      Left e@(SomeException e') ->
        case fromException e of
          Just ExitSuccess -> exitSuccess
          _ -> pure (["Uncaught exception: " ++ displayException e'], 1)
  pure $
    defMessage
      & Fields.exitCode
      .~ exitCode
      & Fields.stderr
      .~ Text.unlines (Text.pack <$> output)
  where
    argv = Text.unpack . decodeUtf8Lenient <$> req.argv

-- | The worker protocol is intended to support streaming events, but we're not using that yet.
streamingNotImplemented :: IO (NextElem (Proto ExecuteEvent)) -> IO (Proto ExecuteResponse)
streamingNotImplemented _ =
  pure $
    defMessage
      & Fields.exitCode
      .~ 1
      & Fields.stderr
      .~ "Streaming not implemented"

-- | Wrap a 'GrpcHandler' in a grapesy handler data type.
fromGrpcHandler ::
  GrpcHandler ->
  Methods IO (ProtobufMethodsOf Worker)
fromGrpcHandler handler =
  simpleMethods
    (mkClientStreaming streamingNotImplemented)
    (mkNonStreaming (execute handler))

