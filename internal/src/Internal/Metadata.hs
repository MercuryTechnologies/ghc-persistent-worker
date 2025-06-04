module Internal.Metadata where

import Control.Concurrent (modifyMVar_, readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import GHC (DynFlags (..), Ghc, GhcMode (..), Logger, ModuleGraph)
import GHC.Driver.Env (HscEnv (..), hscSetActiveUnitId, hscUpdateFlags, hscUpdateLoggerFlags)
import GHC.Driver.Monad (modifySession, modifySessionM, withSession, withTempSession)
import GHC.Driver.Session (updatePlatformConstants)
import GHC.Platform.Ways (Way (WayDyn), addWay)
import GHC.Runtime.Loader (initializeSessionPlugins)
import GHC.Unit (HomeUnit, UnitDatabase, UnitId, UnitState, initUnits)
import GHC.Unit.Env (HomeUnitEnv (..), UnitEnv (..), unitEnv_insert, unitEnv_keys, updateHug)
import GHC.Unit.Home.ModInfo (emptyHomePackageTable)
import Internal.Cache (insertUnitEnv, loadCacheMake, logMemStats, updateModuleGraph)
import Internal.MakeFile (doMkDependHS)
import Internal.Session (Env (..), runSession, withDynFlags)

-- | 'doMkDependHS' needs this to be enabled.
metadataTempSession :: HscEnv -> HscEnv
metadataTempSession =
  hscUpdateFlags \ d -> d {ghcMode = MkDepend, targetWays_ = addWay WayDyn (targetWays_ d)}

insertHomeUnit ::
  UnitId ->
  DynFlags ->
  [UnitDatabase UnitId] ->
  UnitState ->
  HomeUnit ->
  UnitEnv ->
  UnitEnv
insertHomeUnit unit dflags dbs unit_state home_unit unit_env =
  (updateHug (unitEnv_insert unit hue) unit_env) {
    ue_platform = targetPlatform dflags,
    ue_namever = ghcNameVersion dflags
  }
  where
    hue = HomeUnitEnv {
      homeUnitEnv_units = unit_state,
      homeUnitEnv_unit_dbs = Just dbs,
      homeUnitEnv_dflags = dflags,
      homeUnitEnv_hpt = emptyHomePackageTable,
      homeUnitEnv_home_unit = Just home_unit
    }

initHomeUnit :: DynFlags -> Logger -> UnitId -> UnitEnv -> IO UnitEnv
initHomeUnit dflags0 logger unit unit_env = do
  (dbs, unit_state, home_unit, mconstants) <- initUnits logger dflags0 Nothing allUnitIds
  dflags1 <- updatePlatformConstants dflags0 mconstants
  pure (insertHomeUnit unit dflags1 dbs unit_state home_unit unit_env)
  where
    allUnitIds = unitEnv_keys (ue_home_unit_graph unit_env)

-- | Add a new home unit to the current session using the provided 'DynFlags'.
-- The flags have been constructed from Buck CLI args passed to the metadata step, which, crucially, contain the package
-- DB arguments for dependencies.
addHomeUnit :: DynFlags -> Ghc UnitId
addHomeUnit dflags = do
  modifySessionM \ hsc_env -> do
    unit_env <- liftIO $ initHomeUnit dflags hsc_env.hsc_logger unit hsc_env.hsc_unit_env
    pure hsc_env {hsc_unit_env = unit_env}
  pure unit
  where
    unit = dflags.homeUnitId_

-- | Initialize the home unit env for this target and restore the module graphs computed previously for other units.
--
-- This part is the most significant difference that the make worker has from GHC make mode, since it never happens
-- natively that units are added incrementally.
-- Therefore, this is a relatively delicate procedure that hasn't been fully optimized yet.
--
-- We especially want to take care that the command line flags aren't applied to the base session before we initialize
-- the home unit in order to replicate what GHC does in @initMulti@.
prepareMetadataSession :: Env -> DynFlags -> Ghc ()
prepareMetadataSession env dflags = do
  cache <- liftIO $ readMVar env.cache
  modifySessionM \ hsc_env -> liftIO (loadCacheMake env.log hsc_env cache)
  unit <- addHomeUnit dflags
  setActiveUnit unit
  storeNewUnit
  where
    setActiveUnit unit = modifySession (hscUpdateLoggerFlags . hscSetActiveUnitId unit)

    storeNewUnit = withSession \ hsc_env -> liftIO $ modifyMVar_ env.cache (pure . insertUnitEnv hsc_env)

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
computeMetadata :: Env -> IO Bool
computeMetadata env = do
  res <- fmap isJust $ runSession True env $ withDynFlags env \ dflags srcs -> do
    prepareMetadataSession env dflags
    module_graph <- writeMetadata (fst <$> srcs)
    liftIO do
      updateModuleGraph env.cache module_graph
    pure (Just ())
  res <$ logMemStats "after metadata" env.log
