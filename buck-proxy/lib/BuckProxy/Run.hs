module BuckProxy.Run where

import BuckProxy.Orchestration (GhcWorkerCommand (..), WorkerExe (..), WorkerResource (..), proxyServer)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import System.Process (terminateProcess)
import Types.Orchestration (ServerSocketPath (..))


-- | Global options for the worker, passed when the process is started, in contrast to request options stored in
-- 'BuckArgs'.
data CliOptions =
  CliOptions {
    -- | The @ghc-worker@ executable and arguments.
    -- Used to spawn the GHC server, provided by Buck.
    command :: Maybe GhcWorkerCommand,

    -- | If 'True', don't kill the @ghc-worker@ process after the build has concluded (i.e. the @buck-proxy@ process is
    -- terminated by Buck).
    remain :: Bool
  }
  deriving stock (Eq, Show)

defaultCliOptions :: CliOptions
defaultCliOptions =
  CliOptions {
    command = Nothing,
    remain = False
  }

parseOptions :: [String] -> IO CliOptions
parseOptions =
  spin defaultCliOptions
  where
    spin z = \case
      [] -> pure z
      "--exe" : exe : rest -> spin z {command = Just GhcWorkerCommand {exe = WorkerExe exe, args = []}} rest
      "--remain" : rest -> spin z {remain = True} rest
      "--" : args -> pure z {command = z.command <&> \ c -> c {args}}
      arg -> throwIO (userError ("Invalid worker CLI args: " ++ unwords arg))

-- | Main function for starting buck proxy using the provided server socket path and CLI options.
run ::
  -- | This is WORKER_SOCKET
  ServerSocketPath ->
  CliOptions ->
  MVar (IO ()) ->
  IO ()
run socket CliOptions {command, remain} refHandler
  | Nothing <- command
  = throwIO (userError "No ghc-worker executable specified on the command line")
  | Just cmd <- command
  = do
    refWorkerMap <- newMVar (Map.empty)
    -- SIGTERM Handler
    modifyMVar_ refHandler \_ -> pure do
      unless remain do
        wmap <- readMVar refWorkerMap
        for_ wmap \resource ->
          terminateProcess resource.processHandle
    proxyServer refWorkerMap cmd socket
