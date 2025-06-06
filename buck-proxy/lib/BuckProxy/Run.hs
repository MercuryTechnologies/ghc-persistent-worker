module BuckProxy.Run where

import BuckProxy.Orchestration (
  WorkerExe (..),
  WorkerResource (..),
  proxyServer,
  )
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (throwIO)
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import System.Process (terminateProcess)
import Types.GhcHandler (WorkerMode (..))
import Types.Orchestration (
  Orchestration (Multi, Single),
  ServerSocketPath (..),
  )


-- | Global options for the worker, passed when the process is started, in contrast to request options stored in
-- 'BuckArgs'.
data CliOptions =
  CliOptions {
    -- | Should only a single central GHC server be run, with all other worker processes proxying it?
    orchestration :: Orchestration,

    -- | The worker implementation: Make mode or oneshot mode.
    workerMode :: WorkerMode,

    -- | The path to the @buck-worker@ executable.
    -- Usually this is the same executable that started the process, but we cannot access it reliably.
    -- Used to spawn the GHC server, provided by Buck.
    workerExe :: WorkerExe
  }
  deriving stock (Eq, Show)

defaultCliOptions :: CliOptions
defaultCliOptions =
  CliOptions {
    orchestration = Multi,
    workerMode = WorkerOneshotMode,
    workerExe = WorkerExe ""
  }

parseOptions :: [String] -> IO CliOptions
parseOptions =
  spin defaultCliOptions
  where
    spin z = \case
      [] -> pure z
      "--single" : rest -> spin z {orchestration = Single} rest
      "--make" : rest -> spin z {workerMode = WorkerMakeMode} rest
      "--exe" : exe : rest -> spin z {workerExe = WorkerExe exe} rest
      arg -> throwIO (userError ("Invalid worker CLI args: " ++ unwords arg))

-- | Main function for starting buck proxy using the provided server socket path and CLI options.
run ::
  -- | This is WORKER_SOCKET
  ServerSocketPath ->
  CliOptions ->
  MVar (IO ()) ->
  IO ()
run socket CliOptions {workerMode, workerExe} refHandler = do
  refWorkerMap <- newMVar (Map.empty)
  -- SIGTERM Handler
  modifyMVar_ refHandler \_ -> pure do
    wmap <- readMVar refWorkerMap
    for_ wmap \resource ->
      terminateProcess resource.processHandle
  proxyServer refWorkerMap workerExe workerMode socket
