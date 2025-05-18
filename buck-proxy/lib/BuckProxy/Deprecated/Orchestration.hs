{-# LANGUAGE OverloadedStrings #-}

module BuckProxy.Deprecated.Orchestration where

{-
import BuckProxy.Util (dbg)
import qualified BuckWorker as Worker
import BuckWorker (ExecuteCommand, ExecuteResponse)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, putMVar)
import Control.DeepSeq (force)
import Control.Exception (bracket_, throwIO, try)
import Control.Monad (void, when)
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
-}

-- | Path to the worker executable, i.e. this program.
--- Used to spawn the GHC server process.
newtype WorkerExe =
  WorkerExe { path :: FilePath }
  deriving stock (Eq, Show)

{-
-- | Spawn a child process executing the worker executable (which usually is the same as this process), for the purpose
-- of running a GHC server to which all worker processes then forward their requests.
-- Afterwards, wait for the server to be responsive.
forkCentralGhc :: WorkerExe -> WorkerMode -> SocketDirectory -> IO PrimarySocketPath
forkCentralGhc exe mode socketDir = do
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
  waitForCentralGhc ph primary
  pure primary
  where
    primary = primarySocketIn socketDir

-- | Run a server if this process is the primary worker, otherwise return the primary's socket path.
-- Since multiple workers are started in separate processes, we negotiate using file system locks.
-- There are two major scenarios:
--
-- - When the build is started, multiple workers are spawned concurrently, and no primary exists.
--   We create a lock file in the provided socket directory.
--   The first worker that wins the lock in `withFileBlocking` gets to be primary, and writes its socket path to
--   `$socket_dir/primary`.
--   All other workers then own the lock in sequence and proceed with the second scenario.
--
-- - When the build is running and a primary exists, either because the worker lost the inital lock race or was started
--   later in the build due to dependencies and/or parallelism limits, the contents of the `primary` file are read to
--   obtain the primary's socket path.
--   A gRPC server is started that resends all requests to that socket.
runOrProxyCentralGhc ::
  SocketDirectory ->
  (PrimarySocketDiscoveryPath -> IO (PrimarySocketPath, a)) ->
  IO (Either PrimarySocketPath (PrimarySocketPath, a))
runOrProxyCentralGhc socketDir runServer = do
  void $ try @IOError (createDirectoryIfMissing True socketDir.path)
  withFile primaryFile.path ReadWriteMode \ handle -> do
    bracket_ (hLock handle ExclusiveLock) (hUnlock handle) do
      try @IOError (hGetLine handle) >>= \case
        -- If the file didn't exist, `hGetLine` will still return the empty string in some GHC versions.
        -- File IO is buffered/lazy, so we have to force the string to avoid read after close.
        Right !primary | not (null (force primary)) -> do
          pure (Left (PrimarySocketPath primary))
        _ -> do
          (primary, resource) <- runServer primaryFile
          hPutStr handle primary.path
          pure (Right (primary, resource))
  where
    primaryFile = primarySocketDiscoveryIn socketDir

-- | Start a proxy gRPC server that forwards requests to the central GHC server.
-- If that server isn't running, spawn a process and wait for it to boot up.
spawnOrProxyCentralGhc :: WorkerExe -> Orchestration -> WorkerMode -> ServerSocketPath -> MVar (IO ()) -> IO ()
spawnOrProxyCentralGhc exe omode wmode socket refHandler = do
  let socketDir = projectSocketDirectory omode socket
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
