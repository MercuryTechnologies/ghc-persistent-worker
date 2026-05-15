module Types.Orchestration where

import System.Environment (getEnv)
import System.OsPath.Extra ((</>), OsPath, fromOsPath, toOsPath, splitDirectories, takeDirectory)
import System.OsString (intercalate, split, unsafeFromChar)
import Types.Args (TargetId (..))

-- | The file system path of the socket on which the worker running in this process is supposed to listen.
data ServerSocketPath =
  ServerSocketPath {
    path :: OsPath,
    traceId :: String,
    workerSpecId :: String
  }
  deriving stock (Eq, Show)

-- | Extract trace_id and worker_spec_id out of the file path of WORKER_SOCKET.
-- This is a rather hacky way to extract those information but there are no other information when the worker_init
-- step is made.
-- TODO: Make buck2 upstream change to pass this information properly to worker implementation.
extractTraceIdAndWorkerSpecId :: OsPath -> (String, String)
extractTraceIdAndWorkerSpecId sockPath =
  let -- It is of the format: /tmp/buck2_worker/{uuid}-{number}/socket
      ps = splitDirectories sockPath
      str = ps !! 3
      xs = split (unsafeFromChar '-') str
      traceId = fromOsPath $ intercalate (toOsPath "-") $ init xs
      workerSpecId = fromOsPath $ last xs
   in (traceId, workerSpecId)

-- | Given socket path, construct ServerSocketPath
serverSocketFromPath :: OsPath -> ServerSocketPath
serverSocketFromPath path =
  let (traceId, workerSpecId) = extractTraceIdAndWorkerSpecId path
   in ServerSocketPath {path = path, traceId, workerSpecId}

-- | This environment variable is usually set by Buck before starting the worker process.
envServerSocket :: IO ServerSocketPath
envServerSocket = do
  sockPath <- getEnv "WORKER_SOCKET"
  pure $ serverSocketFromPath $ toOsPath sockPath

-- | The base dir for sockets, usually a dir in @/tmp@ created by Buck or ourselves.
newtype SocketDirectory =
  SocketDirectory { path :: OsPath }
  deriving stock (Eq, Show)

-- | Derive the socket base dir from the socket path provided by Buck.
spawnedSocketDirectory :: ServerSocketPath -> SocketDirectory
spawnedSocketDirectory server =
  SocketDirectory (takeDirectory server.path)

-- | The prefix of the socket directory name for the GHC server.
-- Used for manual override on the CLI.
newtype PrimarySocketName =
  PrimarySocketName { path :: OsPath }
  deriving stock (Eq, Show)

-- | For project socket, use the trace id extracted from server socket path.
projectSocketDirectory ::
  -- | base path
  PrimarySocketName ->
  -- | target id.
  TargetId ->
  SocketDirectory
projectSocketDirectory (PrimarySocketName base) targetId = SocketDirectory (root </> workerBase)
  where
    root = toOsPath "/tmp/ghc-persistent-worker"
    workerBase = base <> toOsPath ("_" ++ targetId.string)

-- | The file system path of the socket on which the primary worker running the GHC server is listening.
newtype PrimarySocketPath =
  PrimarySocketPath { path :: OsPath }
  deriving stock (Eq, Show)

-- | For the case where the primary server is spawned, rather than reusing the socket on which communication with Buck
-- is happening.
primarySocketIn :: SocketDirectory -> PrimarySocketPath
primarySocketIn dir = PrimarySocketPath (dir.path </> toOsPath "server")

-- | The file system path of the socket on which the primary worker outputs instrumentation information.
newtype InstrumentSocketPath =
  InstrumentSocketPath { path :: OsPath }
  deriving stock (Eq, Show)

instrumentSocketIn :: SocketDirectory -> InstrumentSocketPath
instrumentSocketIn dir = InstrumentSocketPath (dir.path </> toOsPath "instrument")

-- | The file system path in which the primary worker running the GHC server stores its socket path for clients to
-- discover.
newtype PrimarySocketDiscoveryPath =
  PrimarySocketDiscoveryPath { path :: OsPath }
  deriving stock (Eq, Show)

primarySocketDiscoveryIn :: SocketDirectory -> PrimarySocketDiscoveryPath
primarySocketDiscoveryIn dir = PrimarySocketDiscoveryPath (dir.path </> toOsPath "primary")
