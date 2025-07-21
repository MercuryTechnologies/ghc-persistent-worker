{-# LANGUAGE DeriveAnyClass #-}

module Internal.Metadata where

import Control.Concurrent (MVar, readMVar)
import Control.Exception (throwIO)
import Control.Monad (foldM, (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (StateT (..), gets, modify)
import Data.Aeson (FromJSON (..), eitherDecodeFileStrict')
import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (isJust, listToMaybe)
import Data.Tuple (swap)
import GHC (
  DynFlags (..),
  Ghc,
  GhcMode (..),
  IsBootInterface (..),
  Logger,
  ModuleGraph,
  ModuleName (..),
  getSessionDynFlags,
  mkModuleGraph,
  )
import GHC.Driver.Env (HscEnv (..), hscSetActiveUnitId, hscUpdateFlags, hscUpdateLoggerFlags)
import GHC.Driver.Errors.Types (GhcMessage (..))
import GHC.Driver.Make (ModNodeKeyWithUid (..), summariseFile)
import GHC.Driver.Monad (modifySession, modifySessionM, withSession, withTempSession)
import GHC.Driver.Session (updatePlatformConstants)
import GHC.Generics (Generic)
import GHC.Platform.Ways (Way (WayDyn), addWay)
import GHC.Runtime.Loader (initializeSessionPlugins)
import GHC.Unit (
  GenHomeUnit (..),
  GenWithIsBoot (..),
  HomeUnit,
  UnitDatabase,
  UnitId,
  UnitState,
  initUnits,
  unitIdString,
  )
import GHC.Unit.Env (HomeUnitEnv (..), UnitEnv (..), unitEnv_insert, unitEnv_keys, unitEnv_member, updateHug)
import GHC.Unit.Home.ModInfo (emptyHomePackageTable)
import GHC.Unit.Module.Graph (ModuleGraphNode (..), NodeKey (..))
import GHC.Utils.Outputable (ppr, quotes, text, (<+>))
import Internal.Error (eitherMessages, notePpr)
import Internal.Log (Log, logDebugD, setLogTarget)
import Internal.MakeFile (doMkDependHS)
import Internal.Session (Env (..), buckLocation, parseFlags, runSession, withDynFlags)
import Internal.State (WorkerState (..), modifyMakeState, updateMakeStateVar)
import Internal.State.Make (MakeState (..), insertUnitEnv, loadState, storeModuleGraph)
import Internal.State.Stats (logMemStats)
import Types.Args (Args (..))
import Types.CachedDeps (CachedBuildPlans (..), JsonFs (..))
import Types.State (Target (..))

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

-- | TODO refactor
addHomeUnitTo :: HscEnv -> DynFlags -> IO (HscEnv, UnitId)
addHomeUnitTo hsc_env dflags = do
  unit_env <- liftIO $ initHomeUnit dflags hsc_env.hsc_logger unit hsc_env.hsc_unit_env
  pure (hsc_env {hsc_unit_env = unit_env}, unit)
  where
    unit = dflags.homeUnitId_

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

data CachedModule =
  CachedModule {
    sources :: [FilePath],
    modules :: [JsonFs ModuleName]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data BuckBuildPlan =
  BuckBuildPlan {
    worker_cache :: Map (JsonFs ModuleName) CachedModule,
    unit_args :: Maybe FilePath
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

decodeJsonBuildPlan :: FilePath -> IO BuckBuildPlan
decodeJsonBuildPlan =
  eitherDecodeFileStrict' >=> \case
    Right a -> pure a
    Left err -> throwIO (userError err)

loadCachedModule :: HscEnv -> UnitId -> JsonFs ModuleName -> CachedModule -> IO ModuleGraphNode
loadCachedModule hsc_env unit (JsonFs name) CachedModule {sources, modules} = do
  src <- notePpr "Number of sources /= 1 for module:" name (listToMaybe sources)
  summResult <- summariseFile hsc_env (DefiniteHomeUnit unit Nothing) mempty src Nothing Nothing
  summary <- eitherMessages GhcDriverMessage summResult
  pure (ModuleNode deps summary)
  where
    deps = [NodeKey_Module (ModNodeKeyWithUid (GWIB depName NotBoot) unit) | JsonFs depName <- modules]

loadCachedBuildPlan ::
  MVar Log ->
  HscEnv ->
  DynFlags ->
  UnitId ->
  FilePath ->
  StateT MakeState IO HscEnv
loadCachedBuildPlan logVar hsc_env0 dflags0 unit file = do
  BuckBuildPlan {worker_cache, unit_args} <- liftIO $ decodeJsonBuildPlan file
  maybe (pure hsc_env0) (load worker_cache) unit_args
  where
    load worker_cache args_file = do
      logDebugD logVar (text "Loading cached unit" <+> quotes (ppr unit))
      hsc_env2 <- liftIO do
        args <- readFile args_file
        (dflags1', _, _, _) <- parseFlags dflags0 hsc_env0.hsc_logger (buckLocation <$> lines args)
        let dflags1 = dflags1' {ghcMode = MkDepend, targetWays_ = addWay WayDyn (targetWays_ dflags1')}
        (hsc_env1, _) <- addHomeUnitTo hsc_env0 dflags1
        pure (hscSetActiveUnitId unit hsc_env1)
      modify (insertUnitEnv hsc_env2)
      nodes <- liftIO $ traverse (uncurry (loadCachedModule hsc_env2 unit)) (Map.toList worker_cache)
      modify (storeModuleGraph (mkModuleGraph nodes))
      pure hsc_env2
      -- TODO does not seem necessary, but check with a proper test case
      -- liftIO . loadState logVar hsc_env2 =<< StateT.get

-- TODO toposort the plans, maybe in buck?
loadCachedBuildPlans ::
  MVar Log ->
  MVar WorkerState ->
  DynFlags ->
  Map (JsonFs UnitId) FilePath ->
  HscEnv ->
  IO HscEnv
loadCachedBuildPlans logVar stateVar dflags0 buildPlans hsc_env0 =
  modifyMakeState stateVar $
    fmap swap . runStateT (foldM ensureBuildPlan hsc_env0 (Map.toList buildPlans))
  where
    ensureBuildPlan hsc_env (JsonFs uid, planFile) = do
      present <- gets \ s -> unitEnv_member uid s.hug
      if present
      then pure hsc_env
      else loadCachedBuildPlan logVar hsc_env dflags0 uid planFile

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
computeMetadata :: Env -> IO (Bool, Maybe Target)
computeMetadata env = do
  _ <- runSession True env \ _ -> do
    dflags <- getSessionDynFlags
    for_ env.args.cachedBuildPlans \ (CachedBuildPlans bp) ->
      -- TODO probably unnecessary to modify the session – we reload the graph/unit env anyway
      withSession (liftIO . loadCachedBuildPlans env.log env.state dflags bp)
    -- TODO it appears that json errors aren't fatal – the build just continues
    pure (Just ())
  res <- runSession True env $ withDynFlags env \ dflags srcs -> do
    unit <- prepareMetadataSession env dflags
    let target = Target (unitIdString unit)
    liftIO $ setLogTarget env.log target
    module_graph <- writeMetadata (fst <$> srcs)
    liftIO $ updateMakeStateVar env.state (storeModuleGraph module_graph)
    pure (Just target)
  logMemStats "after metadata" env.log
  pure (isJust res, res)
