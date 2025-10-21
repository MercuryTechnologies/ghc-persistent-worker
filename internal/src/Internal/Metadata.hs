module Internal.Metadata where

import Control.Concurrent (readMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Foldable (for_)
import Data.Maybe (isJust)
import GHC (DynFlags (..), Ghc, GhcMode (..), ModuleGraph, getSession, getSessionDynFlags, setSession)
import GHC.Driver.Env (HscEnv (..), hscSetActiveUnitId, hscUpdateFlags, hscUpdateLoggerFlags)
import GHC.Driver.Monad (modifySession, modifySessionM, withSession, withTempSession)
import GHC.Platform.Ways (Way (WayDyn), addWay)
import GHC.Runtime.Loader (initializeSessionPlugins)
import GHC.Unit (UnitId)
import Internal.Cache.Metadata (addHomeUnitTo, loadCachedUnits)
import Internal.Log (logDebug, logTimed)
import Internal.MakeFile (doMkDependHS)
import Internal.Session (runSession, withDynFlags)
import Internal.State (updateMakeStateVar)
import Internal.State.Make (insertUnitEnv, loadState, storeModuleGraph)
import Internal.State.Stats (logMemStats)
import System.Directory (createDirectoryIfMissing)
import Types.Args (Args (..))
import Types.Env (Env (..))
import Types.Log (Logger (..))
import Types.State (WorkerState (..))
import Types.Target (TargetSpec (..), UnitTarget (..))

-- | 'doMkDependHS' needs this to be enabled.
metadataTempSession :: HscEnv -> HscEnv
metadataTempSession =
  hscUpdateFlags \ d -> d {ghcMode = MkDepend, targetWays_ = addWay WayDyn (targetWays_ d)}

-- | Add a new home unit to the current session using the provided 'DynFlags'.
-- The flags have been constructed from Buck CLI args passed to the metadata step, which, crucially, contain the package
-- DB arguments for dependencies.
addHomeUnit :: DynFlags -> Ghc UnitId
addHomeUnit dflags = do
  hsc_env <- getSession
  (hsc_env1, unit) <- liftIO $ addHomeUnitTo hsc_env dflags
  setSession hsc_env1
  pure unit

-- | Initialize the home unit env for this target and restore the module graphs computed previously for other units.
--
-- This part is the most significant difference that the make worker has from GHC make mode, since it never happens
-- natively that units are added incrementally.
-- Therefore, this is a relatively delicate procedure that hasn't been fully optimized yet.
--
-- We especially want to take care that the command line flags aren't applied to the base session before we initialize
-- the home unit in order to replicate what GHC does in @initMulti@.
prepareMetadataSession :: Env -> DynFlags -> Ghc UnitId
prepareMetadataSession env dflags = do
  state <- liftIO $ readMVar env.state
  modifySessionM \ hsc_env -> liftIO (loadState env.log hsc_env state.make)
  unit <- addHomeUnit dflags
  setActiveUnit unit
  storeNewUnit
  pure unit
  where
    setActiveUnit unit = modifySession (hscUpdateLoggerFlags . hscSetActiveUnitId unit)

    storeNewUnit = withSession \ hsc_env -> liftIO $ updateMakeStateVar env.state (insertUnitEnv hsc_env)

-- | Run 'doMkDependHS' to write the metadata JSON file and exfiltrate the module graph.
-- We need to use a temporary session because 'doMkDependHS' uses some custom settings that we don't want to leak,
-- though it's not been thoroughly tested what precisely the impact is.
writeMetadata :: [String] -> Ghc ModuleGraph
writeMetadata srcs = do
  initializeSessionPlugins
  withTempSession metadataTempSession do
    doMkDependHS srcs

-- | Run downsweep and merge the resulting module graph into the cached graph.
-- This is executed for the metadata step, which natively only calls 'doMkDependHS'.
-- Since that function doesn't give us access to the module graph in its original shape, we inline it into this project
-- to exfiltrate the graph.
-- This has @WayDyn@ hardcoded for now, but it should be adapted to Buck's build configuration.
-- This is usually not necessary (in fact, 'doMkDependHS' clears the target ways) but since we're keeping the module
-- graph the target way will be reflected in the stored @ModSummary@ nodes.
--
-- Before downsweep, we also create a fresh @Finder@ to prevent 'doMkDependHS' from polluting the cache with entries
-- with different compilation ways and restore the previous unit env so dependencies are visible.
computeMetadata :: Env -> IO (Bool, Maybe TargetSpec)
computeMetadata env = do
  res <- runMaybeT do
    () <- MaybeT $ runSession True env \ _ -> do
      dflags <- getSessionDynFlags
      for_ env.args.cachedBuildPlans \ bp ->
        withSession (liftIO . loadCachedUnits env.log env.state dflags bp)
      pure (Just ())
    logTimed env.log "Computing module graph" do
      MaybeT $ runSession True env $ withDynFlags env \ dflags srcs -> do
        unit <- prepareMetadataSession env dflags
        let target = TargetUnit (UnitTarget unit)
        liftIO $ env.log.setTarget target
        module_graph <- writeMetadata (fst <$> srcs)
        liftIO $ updateMakeStateVar env.state (storeModuleGraph module_graph)
        for_ dflags.stubDir \ stubdir -> do
          logDebug env.log ("Creating stubdir: " ++ stubdir)
          liftIO $ createDirectoryIfMissing False stubdir
        pure (Just target)
  logMemStats "after metadata" env.log
  pure (isJust res, res)
