{-# LANGUAGE CPP, NoFieldSelectors #-}

module Internal.State where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, withMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Set (Set)
import GHC (Ghc, ModIface, emptyMG, mi_module, moduleName, moduleNameString, setSession)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Monad (modifySessionM, withSession)
import GHC.Linker.Types (Linkable)
import GHC.Unit.Env (unitEnv_new)
import GHC.Unit.Module.Graph (ModuleGraph)
import Internal.Debug (showHugShort, showModGraph)
import Internal.Log (Log, logDebug, logDebugD)
import qualified Internal.State.Make as Make
import Internal.State.Make (MakeState (..))
import qualified Internal.State.Oneshot as Oneshot
import Internal.State.Oneshot (newOneshotCacheFeatures, newOneshotStateWith)
import qualified Internal.State.Stats as Stats
import System.Environment (lookupEnv)
import Types.Args (TargetId (..))
import Types.Grpc (CommandEnv (..), RequestArgs (..))
import Types.Target (Target, TargetSpec)
import Types.State.Oneshot (OneshotCacheFeatures (..), OneshotState (..))

data ModuleArtifacts =
  ModuleArtifacts {
    iface :: ModIface,
    bytecode :: Maybe Linkable
  }

instance Show ModuleArtifacts where
  show ModuleArtifacts {iface} =
    "ModuleArtifacts { iface = " ++ moduleNameString (moduleName (mi_module iface)) ++ " }"

data BinPath =
  BinPath {
    initial :: Maybe String,
    extra :: Set String
  }
  deriving stock (Eq, Show)

data WorkerState =
  WorkerState {
    path :: BinPath,
    baseSession :: Maybe HscEnv,
    options :: Options,
    make :: MakeState,
    oneshot :: OneshotState,
    targetArgs :: Map TargetSpec (CommandEnv, RequestArgs)
  }

data Options =
  Options {
    extraGhcOptions :: String
  }

newStateWith :: OneshotCacheFeatures -> IO (MVar WorkerState)
newStateWith features = do
  initialPath <- lookupEnv "PATH"
  oneshot <- newOneshotStateWith features
  newMVar WorkerState {
    path = BinPath {
      initial = initialPath,
      extra = mempty
    },
    baseSession = Nothing,
    options = defaultOptions,
    make = MakeState {
      moduleGraph = emptyMG,
      hug = unitEnv_new mempty,
      interp = Nothing
    },
    oneshot,
    targetArgs = mempty
  }

newState :: Bool -> IO (MVar WorkerState)
newState enable = newStateWith newOneshotCacheFeatures {enable}

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

defaultOptions :: Options
defaultOptions =
  Options {
    extraGhcOptions = ""
  }

-- | Log a report for a completed compilation, using 'reportMessages' to assemble the content.
report ::
  MonadIO m =>
  MVar Log ->
  -- | A description of the current worker process.
  Maybe TargetId ->
  Target ->
  WorkerState ->
  m ()
report logVar workerId target state = do
  Stats.report logVar workerId target (if state.oneshot.features.enable then Just state.oneshot.stats else Nothing)

-- | Merge the given module graph into the cached graph.
-- This is used by the make mode worker after the metadata step has computed the module graph.
updateModuleGraph :: MVar WorkerState -> ModuleGraph -> IO ()
updateModuleGraph stateVar new =
  updateMakeStateVar stateVar (Make.storeModuleGraph new)

finalizeCache ::
  MVar Log ->
  -- | A description of the current worker process.
  Maybe TargetId ->
  HscEnv ->
  Target ->
  WorkerState ->
  IO WorkerState
finalizeCache logVar workerId hsc_env target cache0 = do
  oneshot <- Oneshot.storeState hsc_env target cache0.oneshot
  let cache1 = cache0 {oneshot}
  report logVar workerId target cache1
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
  MVar Log ->
  MVar WorkerState ->
  Ghc a ->
  Ghc a
withCacheOneshot workerId target logVar stateVar prog = do
  _ <- withSessionM \ hsc_env -> modifyMVar stateVar \ state -> do
    (oneshot, result) <- Oneshot.loadState (updateOneshotStateVar stateVar) target hsc_env state.oneshot
    pure (state {oneshot}, result)
  result <- prog
  finalize
  pure result
  where
    finalize =
      withSession \ hsc_env ->
        liftIO (modifyMVar_ stateVar (finalizeCache logVar workerId hsc_env target))

-- | This reduced version of 'withCache' is tailored specifically to make mode, only restoring the HUG, module graph and
-- interpreter state from the cache, since those are the only two components modified by the worker that aren't already
-- shared by the base session.
--
-- The mechanisms in 'withCache' are partially legacy experiments whose purpose was to explore which data can be
-- shared manually in oneshot mode, so this variant will be improved more deliberately.
withCacheMake ::
  MVar Log ->
  MVar WorkerState ->
  Ghc a ->
  Ghc a
withCacheMake logVar stateVar prog = do
  modifySessionM restore
  prog <* withSession store
  where
    restore hsc_env =
      liftIO $ modifyMVar stateVar \ state -> do
        (make, hsc_env1) <- Make.loadStateCompile logVar hsc_env state.make
        pure (state {make}, hsc_env1)

    store hsc_env =
      liftIO $ modifyMVar_ stateVar \ state -> do
        make <- Make.storeState logVar hsc_env state.make
        pure state {make}

dumpState ::
  MVar Log ->
  MVar WorkerState ->
  Maybe String ->
  IO ()
dumpState logVar state exception =
  withMVar state \ WorkerState {make = MakeState {moduleGraph, hug}} -> do
    write "-----------------"
    write "Request failed!"
    traverse_ write exception
    write "-----------------"
    write "Module graph:"
    writeD (showModGraph moduleGraph)
    write "-----------------"
    write "Home unit graph:"
    writeD (showHugShort hug)
  where
    write = logDebug logVar
    writeD = logDebugD logVar
