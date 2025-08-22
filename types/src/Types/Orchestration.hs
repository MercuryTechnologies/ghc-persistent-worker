module Types.Orchestration where

import Data.List (intersperse)
import Data.List.Split (splitOn)
import System.Environment (getEnv)
import System.FilePath (splitDirectories, takeDirectory, (</>))
import Types.Args (TargetId (..))

-- | The file system path of the socket on which the worker running in this process is supposed to listen.
data ServerSocketPath =
  ServerSocketPath {
    path :: FilePath,
    traceId :: String,
    workerSpecId :: String
  }
  deriving stock (Eq, Show)

-- | Extract trace_id and worker_spec_id out of the file path of WORKER_SOCKET.
-- This is a rather hacky way to extract those information but there are no other information when the worker_init
-- step is made.
-- TODO: Make buck2 upstream change to pass this information properly to worker implementation.
extractTraceIdAndWorkerSpecId :: FilePath -> (String, String)
extractTraceIdAndWorkerSpecId sockPath =
  let -- It is of the format: /tmp/buck2_worker/{uuid}-{number}/socket
      ps = splitDirectories sockPath
      str = ps !! 3
      xs = splitOn "-" str
      traceId = concat $ intersperse "-" $ init xs
      workerSpecId = last xs
   in (traceId, workerSpecId)

-- | Given socket path, construct ServerSocketPath
serverSocketFromPath :: FilePath -> ServerSocketPath
serverSocketFromPath path =
  let (traceId, workerSpecId) = extractTraceIdAndWorkerSpecId path
   in ServerSocketPath {path, traceId, workerSpecId}

-- | This environment variable is usually set by Buck before starting the worker process.
envServerSocket :: IO ServerSocketPath
envServerSocket = do
  sockPath <- getEnv "WORKER_SOCKET"
  pure (serverSocketFromPath sockPath)

-- | The base dir for sockets, usually a dir in @/tmp@ created by Buck or ourselves.
newtype SocketDirectory =
  SocketDirectory { path :: FilePath }
  deriving stock (Eq, Show)

-- | Derive the socket base dir from the socket path provided by Buck.
spawnedSocketDirectory :: ServerSocketPath -> SocketDirectory
spawnedSocketDirectory server =
  SocketDirectory (takeDirectory server.path)

-- | For project socket, use the trace id extracted from server socket path.
projectSocketDirectory ::
  -- | base path
  FilePath ->
  -- | target id.
  TargetId ->
  SocketDirectory
projectSocketDirectory base targetId = SocketDirectory (root </> workerBase)
  where
    root = "/tmp/ghc-persistent-worker"
    workerBase = base ++ "_" ++ targetId.string

-- | The file system path of the socket on which the primary worker running the GHC server is listening.
newtype PrimarySocketPath =
  PrimarySocketPath { path :: FilePath }
  deriving stock (Eq, Show)

-- | For the case where the primary server is spawned, rather than reusing the socket on which communication with Buck
-- is happening.
primarySocketIn :: SocketDirectory -> PrimarySocketPath
primarySocketIn dir = PrimarySocketPath (dir.path </> "server")

-- | The file system path of the socket on which the primary worker outputs instrumentation information.
newtype InstrumentSocketPath =
  InstrumentSocketPath { path :: FilePath }
  deriving stock (Eq, Show)

instrumentSocketIn :: SocketDirectory -> InstrumentSocketPath
instrumentSocketIn dir = InstrumentSocketPath (dir.path </> "instrument")

-- | The file system path in which the primary worker running the GHC server stores its socket path for clients to
-- discover.
newtype PrimarySocketDiscoveryPath =
  PrimarySocketDiscoveryPath { path :: FilePath }
  deriving stock (Eq, Show)

primarySocketDiscoveryIn :: SocketDirectory -> PrimarySocketDiscoveryPath
primarySocketDiscoveryIn dir = PrimarySocketDiscoveryPath (dir.path </> "primary")
