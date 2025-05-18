module BuckProxy.Orchestration (
  WorkerExe (..),
  proxyServer,
  spawnGhcWorker,
) where

import BuckProxy.Util (dbg)
import qualified BuckWorker as Worker
import BuckWorker (ExecuteCommand, ExecuteResponse)
import Common.Grpc (GrpcHandler (..), fromGrpcHandler, streamingNotImplemented)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, putMVar, takeMVar)
import Control.DeepSeq (force)
import Control.Exception (bracket_, throwIO, try)
import Control.Monad (void, when)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import GHC.IO.Handle.Lock (LockMode (..), hLock, hUnlock)
import Network.GRPC.Client (Connection, Server (..), recvNextOutput, sendFinalInput, withConnection, withRPC)
import Network.GRPC.Common (NextElem (..), Proxy (..), def)
import Network.GRPC.Common.Protobuf (Proto, Protobuf, defMessage, (%~), (&), (.~))
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.Run (InsecureConfig (..), ServerConfig (..), runServerWithHandlers)
import Network.GRPC.Server.StreamType (Methods (..), fromMethods, mkClientStreaming, mkNonStreaming)
import Proto.Worker (ExecuteEvent, Worker (..))
import Proto.Worker_Fields qualified as Fields
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.Exit (exitFailure)
import System.IO (IOMode (..), hGetLine, hPutStr, openFile, withFile)
import System.Process (CreateProcess (..), ProcessHandle, StdStream(UseHandle), createProcess, getProcessExitCode, proc)
import Types.BuckArgs (BuckArgs (workerTargetId), parseBuckArgs)
import Types.GhcHandler (WorkerMode (..))
import Types.Orchestration (
  Orchestration,
  PrimarySocketDiscoveryPath (..),
  PrimarySocketPath (..),
  ServerSocketPath (..),
  SocketDirectory (..),
  primarySocketDiscoveryIn,
  primarySocketIn,
  projectSocketDirectory,
  )

-- | Path to the worker executable, i.e. this program.
--- Used to spawn the GHC server process.
newtype WorkerExe =
  WorkerExe { path :: FilePath }
  deriving stock (Eq, Show)

-- | Forward a request received from a client to another gRPC server and forward the response back,
-- prefixing the error messages so we know where the error originated.
forwardRequest ::
  {- Connection -> -}
  Proto ExecuteCommand ->
  IO (Proto ExecuteResponse)
forwardRequest {- connection -} req = do
  dbg (show req)
  pure defMessage

  {- withRPC connection def (Proxy @(Protobuf Worker "execute")) \ call -> do
  sendFinalInput call req
  resp <- recvNextOutput call
  pure $
    resp
      & Fields.stderr
      %~ ("gRPC client error: " <>)
  -}

proxyHandler ::
  MVar (Map String PrimarySocketPath) ->
  WorkerExe ->
  Orchestration ->
  WorkerMode ->
  ServerSocketPath ->
  GrpcHandler
proxyHandler workerMap exe omode wmode socket =
  GrpcHandler \ commandEnv argv -> do
    buckArgs <- either (throwIO . userError) pure (parseBuckArgs commandEnv argv)
    let mtargetId = buckArgs.workerTargetId
    case mtargetId of
      Nothing -> throwIO (userError "No --worker-target-id passed")
      Just targetId -> do
        wmap <- takeMVar workerMap
        case Map.lookup targetId wmap of
          Nothing -> do
            let workerSocketDir = projectSocketDirectory targetId
            void $ try @IOError (createDirectoryIfMissing True workerSocketDir.path)
            primary <- spawnGhcWorker exe wmode workerSocketDir
            putMVar workerMap (Map.insert targetId primary wmap)
            let msg = "No primary socket for " ++ show targetId ++ ", so created it on " ++ primary.path
            pure ([msg], 1)
          Just primary -> do
            let msg = "Primary socket for " ++ show targetId ++ ": " ++ primary.path
            pure ([msg], 1)

{-
-- | Bracket a computation with a gRPC worker client resource, connecting to the server listening on the provided
-- socket.
withProxy ::
  -- PrimarySocketPath ->
  MVar (Map String PrimarySocketPath) ->
  (Methods IO (ProtobufMethodsOf Worker) -> IO a) ->
  IO a
withProxy workerMap use = do
  -- withConnection def server $ \ connection -> do
    use $ fromGrpcHandler (proxyHandler workerMap )

      {-
      Method (mkClientStreaming streamingNotImplemented) $
      Method (mkNonStreaming (forwardRequest {- connection -})) $
      NoMoreMethods -}
  {- where
    server = ServerUnix socket.path
  -}
-}

grpcServerConfig :: FilePath -> ServerConfig
grpcServerConfig socketPath =
  ServerConfig
    { serverInsecure = Just (InsecureUnix socketPath)
    , serverSecure = Nothing
    }

-- | Start a worker gRPC server that forwards requests received from a client (here Buck) to ghc-worker
proxyServer ::
  -- | mutable worker map (we spawn a new ghc-worker as a new target id arrives)
  MVar (Map String PrimarySocketPath) ->
  WorkerExe ->
  Orchestration ->
  WorkerMode ->
  ServerSocketPath ->
  IO ()
proxyServer workerMap exe omode wmode socket = do
  try launch >>= \case
    Right () ->
      dbg ("Shutting down buck-proxy on " ++ socket.path)
    Left (err :: IOError) -> do
      dbg ("buck-proxy on" ++ socket.path ++ " crashed" ++ show err)
      exitFailure
  where
    methods = fromGrpcHandler (proxyHandler workerMap exe omode wmode socket)
    launch = do
      dbg ("Starting buck-proxy on " ++ socket.path)
      runServerWithHandlers def (grpcServerConfig socket.path) $ fromMethods methods

messageExecute :: Proto Worker.ExecuteCommand
messageExecute = defMessage

-- | How often the process should wait for 100ms and retry connecting to the GHC server after spawning a process.
maxRetries :: Int
maxRetries = 30

-- | Attempt to connect and send a gRPC message to the server starting up at the given socket.
waitPoll :: PrimarySocketPath -> IO ()
waitPoll socket =
  check maxRetries
  where
    check 0 = throwIO (userError "GHC server didn't respond within 3 seconds")
    check n =
      try connect >>= \case
        Right () -> pure ()
        Left (_ :: IOError) -> do
          threadDelay 100_000
          check (n - 1)

    -- The part that throws is in @withConnection@, so this has to be executed every time.
    connect =
      withConnection def (ServerUnix socket.path) \ connection ->
        withRPC connection def (Proxy @(Protobuf Worker "execute")) \ call ->
          sendFinalInput call messageExecute <* recvNextOutput call

-- | Wait for a GHC server process to respond and check its exit code.
waitForGhcWorker :: ProcessHandle -> PrimarySocketPath -> IO ()
waitForGhcWorker ph socket = do
  dbg "Waiting for server"
  waitPoll socket
  dbg "Server is up"
  exitCode <- getProcessExitCode ph
  when (isJust exitCode) do
    dbg "Spawned process for the GHC server exited after starting up."

-- | Spawn a child process executing the worker executable (which usually is the same as this process), for the purpose
-- of running a GHC server to which all worker processes then forward their requests.
-- Afterwards, wait for the server to be responsive.
spawnGhcWorker ::
  WorkerExe ->
  WorkerMode ->
  SocketDirectory ->
  IO PrimarySocketPath
spawnGhcWorker exe mode socketDir = do
  dbg ("Forking GHC server at " ++ primary.path)
  -- let fp = takeDirectory primary.path </> "stderr"
  -- fh <- openFile fp WriteMode
  let workerModeFlag = case mode of
        WorkerOneshotMode -> []
        WorkerMakeMode -> ["--make"]
      worker = (proc exe.path (workerModeFlag ++ ["--serve", primary.path]))
        -- { std_err = UseHandle fh
        -- }
  (_, _, _, ph) <- createProcess worker
  waitForGhcWorker ph primary
  pure primary
  where
    primary = primarySocketIn socketDir


{-
-- | Start a proxy gRPC server that forwards requests to the central GHC server.
-- If that server isn't running, spawn a process and wait for it to boot up.
spawnOrProxyCentralGhc ::
  MVar (Map String FilePath) ->
  WorkerExe ->
  Orchestration ->
  WorkerMode ->
  ServerSocketPath ->
  MVar (IO ()) ->
  IO ()
spawnOrProxyCentralGhc workerMap exe omode wmode socket refHandler = undefined
-}
{-  let socketDir = projectSocketDirectory omode socket
  eprimary <- runOrProxyCentralGhc socketDir \ _ -> do
    primary <- forkCentralGhc exe wmode socketDir
    pure (primary, ())
  let primary = either id fst eprimary
  putMVar refHandler $ do
    dbg (show primary)
    withConnection def (ServerUnix primary.path) \ connection ->
      withRPC connection def (Proxy @(Protobuf Worker "execute")) \ call -> do
        let req = defMessage & Fields.argv .~ ["--worker-mode", "terminate"]
        sendFinalInput call req
        resp <- recvNextOutput call
        dbg (show resp)

  proxyServer primary socket
-}
