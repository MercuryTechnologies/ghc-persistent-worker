{-# LANGUAGE CPP, NoFieldSelectors #-}

module Internal.Cache where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Set (Set)
import GHC (Ghc, ModIface, emptyMG, mi_module, moduleName, moduleNameString, setSession)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Monad (modifySessionM, withSession)
import GHC.Linker.Types (Linkable)
import GHC.Unit.Env (unitEnv_new)
import GHC.Unit.Module.Graph (ModuleGraph)
import Internal.Log (Log)
import qualified Internal.State.Make as Make
import Internal.State.Make (MakeState (..))
import qualified Internal.State.Oneshot as Oneshot
import Internal.State.Oneshot (OneshotCacheFeatures (..), OneshotState, newOneshotCacheFeatures, newOneshotStateWith)
import qualified Internal.State.Stats as Stats
import System.Environment (lookupEnv)
import Types.Args (TargetId (..))
import Types.State (Target)

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

data Cache =
  Cache {
    path :: BinPath,
    baseSession :: Maybe HscEnv,
    options :: Options,
    make :: MakeState,
    oneshot :: OneshotState
  }

data Options =
  Options {
    extraGhcOptions :: String
  }

emptyCacheWith :: OneshotCacheFeatures -> IO (MVar Cache)
emptyCacheWith features = do
  initialPath <- lookupEnv "PATH"
  oneshot <- newOneshotStateWith features
  newMVar Cache {
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
    oneshot
  }

emptyCache :: Bool -> IO (MVar Cache)
emptyCache enable = do
  emptyCacheWith newOneshotCacheFeatures {enable}

-- | Update the 'MakeState' field in the 'Cache'.
updateMakeState :: (MakeState -> MakeState) -> Cache -> Cache
updateMakeState f cache = cache {make = f cache.make}

updateMakeStateVar :: MVar Cache -> (MakeState -> MakeState) -> IO ()
updateMakeStateVar var f = modifyMVar_ var (pure . updateMakeState f)

updateOneshotState :: (OneshotState -> OneshotState) -> Cache -> Cache
updateOneshotState f cache = cache {oneshot = f cache.oneshot}

updateOneshotStateVar :: MVar Cache -> (OneshotState -> OneshotState) -> IO ()
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
  Cache ->
  m ()
report logVar workerId target cache = do
  Stats.report logVar workerId target (if cache.oneshot.features.enable then Just cache.oneshot.stats else Nothing)

-- | Merge the given module graph into the cached graph.
-- This is used by the make mode worker after the metadata step has computed the module graph.
updateModuleGraph :: MVar Cache -> ModuleGraph -> IO ()
updateModuleGraph cacheVar new =
  updateMakeStateVar cacheVar (Make.storeModuleGraph new)

finalizeCache ::
  MVar Log ->
  -- | A description of the current worker process.
  Maybe TargetId ->
  HscEnv ->
  Target ->
  Maybe ModuleArtifacts ->
  Cache ->
  IO Cache
finalizeCache logVar workerId hsc_env target _ cache0 = do
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
  MVar Log ->
  -- | A description of the current worker process.
  Maybe TargetId ->
  MVar Cache ->
  Target ->
  Ghc (Maybe (Maybe ModuleArtifacts, a)) ->
  Ghc (Maybe (Maybe ModuleArtifacts, a))
withCacheOneshot logVar workerId cacheVar target prog = do
  _ <- withSessionM \ hsc_env -> modifyMVar cacheVar \ cache -> do
    (oneshot, result) <- Oneshot.loadState (updateOneshotStateVar cacheVar) target hsc_env cache.oneshot
    pure (cache {oneshot}, result)
  result <- prog
  finalize (fst =<< result)
  pure result
  where
    finalize art =
      withSession \ hsc_env ->
        liftIO (modifyMVar_ cacheVar (finalizeCache logVar workerId hsc_env target art))

-- | This reduced version of 'withCache' is tailored specifically to make mode, only restoring the HUG, module graph and
-- interpreter state from the cache, since those are the only two components modified by the worker that aren't already
-- shared by the base session.
--
-- The mechanisms in 'withCache' are partially legacy experiments whose purpose was to explore which data can be
-- shared manually in oneshot mode, so this variant will be improved more deliberately.
withCacheMake ::
  MVar Log ->
  MVar Cache ->
  Ghc (Maybe (Maybe ModuleArtifacts, a)) ->
  Ghc (Maybe (Maybe ModuleArtifacts, a))
withCacheMake logVar cacheVar prog = do
  modifySessionM restore
  prog <* withSession store
  where
    restore hsc_env =
      liftIO $ modifyMVar cacheVar \ cache -> do
        (make, hsc_env1) <- Make.loadStateCompile logVar hsc_env cache.make
        pure (cache {make}, hsc_env1)

    store hsc_env =
      liftIO $ modifyMVar_ cacheVar \ cache -> do
        make <- Make.storeState logVar hsc_env cache.make
        pure cache {make}
