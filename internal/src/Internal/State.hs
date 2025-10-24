{-# LANGUAGE CPP, NoFieldSelectors #-}

module Internal.State where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, readMVar, withMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import GHC (Ghc, ModIface, mi_module, moduleName, moduleNameString, setSession)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Monad (modifySessionM, withSession)
import GHC.Linker.Types (Linkable)
import GHC.Unit.Module.Graph (ModuleGraph)
import Internal.Debug (showHugShort, showModGraph)
import Internal.Log (logDebug, logDebugD)
import qualified Internal.State.Make as Make
import qualified Internal.State.Oneshot as Oneshot
import qualified Internal.State.Stats as Stats
import Types.Args (TargetId (..))
import Types.Log (Logger)
import Types.State (WorkerState (..))
import Types.State.Make (MakeState (..))
import Types.State.Oneshot (OneshotCacheFeatures (..), OneshotState (..))
import Types.Target (Target)

data ModuleArtifacts =
  ModuleArtifacts {
    iface :: ModIface,
    bytecode :: Maybe Linkable
  }

instance Show ModuleArtifacts where
  show ModuleArtifacts {iface} =
    "ModuleArtifacts { iface = " ++ moduleNameString (moduleName (mi_module iface)) ++ " }"

modifyMakeState :: MVar WorkerState -> (MakeState -> IO (MakeState, a)) -> IO a
modifyMakeState var f =
  modifyMVar var \ state -> do
    (make, a) <- f state.make
    pure (state {make}, a)

-- | Update the 'MakeState' field in the 'WorkerState'.
updateMakeState :: (MakeState -> MakeState) -> WorkerState -> WorkerState
updateMakeState f state = state {make = f state.make}

updateMakeStateVar :: MVar WorkerState -> (MakeState -> MakeState) -> IO ()
updateMakeStateVar var f = modifyMakeState var (\ s -> pure (f s, ()))

updateOneshotState :: (OneshotState -> OneshotState) -> WorkerState -> WorkerState
updateOneshotState f state = state {oneshot = f state.oneshot}

updateOneshotStateVar :: MVar WorkerState -> (OneshotState -> OneshotState) -> IO ()
updateOneshotStateVar var f = modifyMVar_ var (pure . updateOneshotState f)

withMakeState ::
  MVar WorkerState ->
  (MakeState -> IO a) ->
  IO a
withMakeState var f = do
  WorkerState {make} <- readMVar var
  f make

-- | Log a report for a completed compilation, using 'reportMessages' to assemble the content.
report ::
  MonadIO m =>
  Logger ->
  -- | A description of the current worker process.
  Maybe TargetId ->
  Target ->
  WorkerState ->
  m ()
report logger workerId target state = do
  Stats.report logger workerId target (if state.oneshot.features.enable then Just state.oneshot.stats else Nothing)

-- | Merge the given module graph into the cached graph.
-- This is used by the make mode worker after the metadata step has computed the module graph.
updateModuleGraph :: MVar WorkerState -> ModuleGraph -> IO ()
updateModuleGraph stateVar new =
  updateMakeStateVar stateVar (Make.storeModuleGraph new)

finalizeCache ::
  Logger ->
  -- | A description of the current worker process.
  Maybe TargetId ->
  HscEnv ->
  Target ->
  WorkerState ->
  IO WorkerState
finalizeCache logger workerId hsc_env target cache0 = do
  oneshot <- Oneshot.storeState hsc_env target cache0.oneshot
  let cache1 = cache0 {oneshot}
  report logger workerId target cache1
  pure cache1

withSessionM :: (HscEnv -> IO (HscEnv, a)) -> Ghc a
withSessionM use =
  withSession \ hsc_env -> do
    (new_env, a) <- liftIO $ use hsc_env
    setSession new_env
    pure a

withCacheOneshot ::
  -- | A description of the current worker process.
  Maybe TargetId ->
  Target ->
  Logger ->
  MVar WorkerState ->
  Ghc a ->
  Ghc a
withCacheOneshot workerId target logger stateVar prog = do
  _ <- withSessionM \ hsc_env -> modifyMVar stateVar \ state -> do
    (oneshot, result) <- Oneshot.loadState (updateOneshotStateVar stateVar) target hsc_env state.oneshot
    pure (state {oneshot}, result)
  result <- prog
  finalize
  pure result
  where
    finalize =
      withSession \ hsc_env ->
        liftIO (modifyMVar_ stateVar (finalizeCache logger workerId hsc_env target))

-- | This reduced version of 'withCache' is tailored specifically to make mode, only restoring the HUG, module graph and
-- interpreter state from the cache, since those are the only two components modified by the worker that aren't already
-- shared by the base session.
--
-- The mechanisms in 'withCache' are partially legacy experiments whose purpose was to explore which data can be
-- shared manually in oneshot mode, so this variant will be improved more deliberately.
withCacheMake ::
  Logger ->
  MVar WorkerState ->
  Ghc a ->
  Ghc a
withCacheMake logger stateVar prog = do
  modifySessionM restore
  prog <* withSession store
  where
    restore hsc_env =
      liftIO $ modifyMVar stateVar \ state -> do
        (make, hsc_env1) <- Make.loadStateCompile logger hsc_env state.make
        pure (state {make}, hsc_env1)

    store hsc_env =
      liftIO $ modifyMVar_ stateVar \ state -> do
        make <- Make.storeState logger hsc_env state.make
        pure state {make}

dumpState ::
  Logger ->
  MVar WorkerState ->
  Maybe String ->
  IO ()
dumpState logger state exception =
  withMVar state \ WorkerState {make = MakeState {moduleGraph, hug}} -> do
    write "-----------------"
    write "Request failed!"
    traverse_ write exception
    write "-----------------"
    write "Module graph:"
    writeD (showModGraph moduleGraph)
    write "-----------------"
    write "Home unit graph:"
    writeD =<< showHugShort hug
  where
    write = logDebug logger
    writeD = logDebugD logger
