module Common.Grpc (
  GrpcHandler (..),
  debugRequestArgs,
  commandEnv,
  execute,
  streamingNotImplemented,
  fromGrpcHandler,
  grpcServerConfig,
  runGrpcServer,
  forwardRequest,
  sendRequest,
  waitPoll,
) where

import BuckWorkerProto (ExecuteCommand, ExecuteResponse)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException (..), displayException, fromException, throwIO, try)
import Control.Monad (when)
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import Network.GRPC.Client (Connection, Server (..), recvNextOutput, sendFinalInput, withConnection, withRPC)
import Network.GRPC.Common (NextElem (..), Proxy (..), def)
import Network.GRPC.Common.Protobuf (Proto, Protobuf, defMessage, (%~), (&), (.~))
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.Run (InsecureConfig (..), ServerConfig (..), runServerWithHandlers)
import Network.GRPC.Server.StreamType (Methods (..), fromMethods, mkClientStreaming, mkNonStreaming, simpleMethods)
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

-- | Create a gRPC server config listening on a Unix domain socket.
grpcServerConfig :: FilePath -> ServerConfig
grpcServerConfig socketPath =
  ServerConfig
    { serverInsecure = Just (InsecureUnix socketPath)
    , serverSecure = Nothing
    }

-- | Run a gRPC server on a Unix domain socket with the given methods.
runGrpcServer ::
  FilePath ->
  Methods IO rpcs ->
  IO ()
runGrpcServer socketPath methods =
  runServerWithHandlers def (grpcServerConfig socketPath) (fromMethods methods)

-- | Send an 'ExecuteCommand' to the 'Server' on a new connection and return the response.
sendRequest :: Server -> Proto ExecuteCommand -> IO (Proto ExecuteResponse)
sendRequest server request =
  withConnection def server \ connection ->
    withRPC connection def (Proxy @(Protobuf Worker "execute")) \ call -> do
      sendFinalInput call request
      recvNextOutput call

-- | Forward a request received from a client to another gRPC server and forward the response back,
-- prefixing the error messages so we know where the error originated.
forwardRequest ::
  Connection ->
  Proto ExecuteCommand ->
  IO (Proto ExecuteResponse)
forwardRequest connection req =
  withRPC connection def (Proxy @(Protobuf Worker "execute")) \ call -> do
    sendFinalInput call req
    resp <- recvNextOutput call
    pure $
      resp
        & Fields.stderr
        %~ ("gRPC client error: " <>)

-- | Attempt to connect and send a gRPC message to the server at the given socket path.
-- Retries up to 30 times with 100ms delay (3 seconds total).
waitPoll :: FilePath -> IO ()
waitPoll socketPath =
  check maxRetries
  where
    maxRetries :: Int
    maxRetries = 30

    check 0 = throwIO (userError "GHC server didn't respond within 3 seconds")
    check n =
      try connect >>= \case
        Right _ -> pure ()
        Left (_ :: IOError) -> do
          threadDelay 100_000
          check (n - 1)

    -- The part that throws is in @withConnection@, so this has to be executed every time.
    connect = sendRequest (ServerUnix socketPath) messageExecute

    messageExecute :: Proto ExecuteCommand
    messageExecute = defMessage
