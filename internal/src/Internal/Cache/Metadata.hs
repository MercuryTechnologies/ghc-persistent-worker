{-# LANGUAGE CPP #-}
#define RECENT (MIN_VERSION_GLASGOW_HASKELL(9,13,0,0) || defined(MWB_2025_10))

module Internal.Cache.Metadata where

import Control.Concurrent (MVar, modifyMVar)
import Control.Exception (throwIO)
import Control.Monad (foldM, (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (StateT (..), gets, modify, modifyM)
import Data.Aeson (eitherDecodeFileStrict')
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Tuple (swap)
import qualified GHC
import GHC (DynFlags (..), IsBootInterface (..), ModuleName (..), mkModuleGraph)
import GHC.Driver.Env (HscEnv (..), hscSetActiveUnitId)
import GHC.Driver.Make (ModNodeKeyWithUid (..))
import GHC.Driver.Session (updatePlatformConstants)
import GHC.Unit (GenWithIsBoot (..), HomeUnit, UnitDatabase, UnitId, UnitState, initUnits)
import GHC.Unit.Env (HomeUnitEnv (..), UnitEnv (..), updateHug)
import GHC.Unit.Home (GenHomeUnit (DefiniteHomeUnit))
import GHC.Unit.Module.Graph (ModuleGraphNode (..), NodeKey (..))
import GHC.Utils.Outputable (ppr, quotes, text, (<+>))
import Internal.Error (notePpr)
import Internal.Log (logDebugD, logTimed, logTimedD)
import Internal.Session (buckLocation, parseFlags, setupPath)
import Internal.State (updateMakeState)
import Internal.State.Make (insertUnitEnv, storeModuleGraph)
import Internal.UnitEnv (emptyHomePackageTable)
import Types.BuckArgs (CachedBuckArgs (..), parseCachedBuckArgs)
import Types.CachedDeps (CachedBuildPlan (..), CachedBuildPlans (..), CachedModule (..), CachedUnit (..), JsonFs (..))
import Types.Log (Logger (..))
import Types.State (WorkerState (..))
import Types.State.Make (MakeState (..))

#if RECENT

import GHC.Data.OsPath (unsafeEncodeUtf)
import GHC.Driver.Config.Finder (initFinderOpts)
import GHC.Types.SourceFile (HscSource (HsSrcFile))
import GHC.Unit.Finder (addHomeModuleToFinder, mkHomeModLocation)
import GHC.Unit.Home.Graph (unitEnv_insert, unitEnv_keys, unitEnv_lookup_maybe)
import GHC.Unit.Module.Graph (ModuleNodeInfo (..))
import System.FilePath (splitExtension)

#else

import GHC.Driver.Errors.Types (GhcMessage (..))
import GHC.Driver.Make (summariseFile)
import GHC.Unit.Env (unitEnv_insert, unitEnv_keys, unitEnv_lookup_maybe)
import Internal.Error (eitherMessages)

#endif

-- | Add a fresh 'HomeUnitEnv' to the home unit graph using the supplied unit state and dependencies.
insertHomeUnit ::
  UnitId ->
  DynFlags ->
  [UnitDatabase UnitId] ->
  UnitState ->
  HomeUnit ->
  UnitEnv ->
  IO UnitEnv
insertHomeUnit unit dflags dbs unit_state home_unit unit_env = do
  hpt <- emptyHomePackageTable
  pure (updateHug (unitEnv_insert unit (hue hpt)) unit_env) {
    ue_platform = targetPlatform dflags,
    ue_namever = ghcNameVersion dflags
  }
  where
    hue homeUnitEnv_hpt = HomeUnitEnv {
      homeUnitEnv_units = unit_state,
      homeUnitEnv_unit_dbs = Just dbs,
      homeUnitEnv_dflags = dflags,
      homeUnitEnv_hpt,
      homeUnitEnv_home_unit = Just home_unit
    }

-- | Create a new home unit using the supplied 'DynFlags'.
initHomeUnit :: DynFlags -> GHC.Logger -> UnitId -> UnitEnv -> IO UnitEnv
initHomeUnit dflags0 logger unit unit_env = do
  (dbs, unit_state, home_unit, mconstants) <- initUnits logger dflags0 Nothing allUnitIds
  dflags1 <- updatePlatformConstants dflags0 mconstants
  insertHomeUnit unit dflags1 dbs unit_state home_unit unit_env
  where
    allUnitIds = unitEnv_keys (ue_home_unit_graph unit_env)

-- | Add a new home unit to the given session using the provided 'DynFlags'.
-- The flags have been constructed from Buck CLI args passed to the metadata step, which, crucially, contain the package
-- DB arguments for dependencies.
addHomeUnitTo :: HscEnv -> DynFlags -> IO (HscEnv, UnitId)
addHomeUnitTo hsc_env dflags = do
  unit_env <- liftIO $ initHomeUnit dflags hsc_env.hsc_logger unit hsc_env.hsc_unit_env
  pure (hsc_env {hsc_unit_env = unit_env}, unit)
  where
    unit = dflags.homeUnitId_

decodeJsonBuildPlan :: FilePath -> IO CachedUnit
decodeJsonBuildPlan =
  eitherDecodeFileStrict' >=> \case
    Right a -> pure a
    Left err -> throwIO (userError err)

-- | This doesn't restore any non-home-unit dependencies. I'm not sure why...maybe those aren't needed for cached
-- modules.
-- I would expect TH to fail, but it appear not to.
loadCachedModule :: HscEnv -> UnitId -> JsonFs ModuleName -> CachedModule -> IO ModuleGraphNode
loadCachedModule hsc_env unit (JsonFs name) CachedModule {sources, modules} = do
  src <- notePpr "Number of sources /= 1 for module:" name (listToMaybe sources)
  node <- createNode src
  pure (ModuleNode deps node)
  where
    deps = [NodeKey_Module (ModNodeKeyWithUid (GWIB depName NotBoot) unit) | JsonFs depName <- modules]

#if RECENT
    createNode src = do
      _ <- addHomeModuleToFinder hsc_env.hsc_FC (DefiniteHomeUnit unit Nothing) name location HsSrcFile
      pure $ ModuleNodeFixed (ModNodeKeyWithUid (GWIB name NotBoot) unit) location
      where
        fopts = initFinderOpts (hsc_dflags hsc_env)
        (basename, extension) = splitExtension src
        location = mkHomeModLocation fopts name (unsafeEncodeUtf basename) (unsafeEncodeUtf extension) HsSrcFile
#else
    createNode src = do
      summResult <- summariseFile hsc_env (DefiniteHomeUnit unit Nothing) mempty src Nothing Nothing
      eitherMessages GhcDriverMessage summResult
#endif

-- | Restore non-GHC state from the Buck cache.
-- Command line arguments interpreted directly by the worker aren't part of the unit args, but they can yet influence
-- the behavior of unit state restoration from cache.
--
-- At the moment, this only includes the @$PATH@ variable, which is relevant even when restoring the module graph from
-- cache because we're parsing the source code, which might include executing preprocessors.
loadCachedArgs ::
  FilePath ->
  StateT WorkerState IO ()
loadCachedArgs path = do
  cachedArgs <- liftIO $ readFile path
  case parseCachedBuckArgs (lines cachedArgs) of
    Right args -> modifyM (setupPath args.cachedBinPath)
    Left err -> liftIO $ throwIO (userError err)

-- | Restore the unit state and module graph from the external cache.
--
-- The cached data consists of a simple list of GHC command line arguments that can recreate the unit state, as well as
-- the module graph produced by a previous metadata request.
loadCachedUnit ::
  Logger ->
  HscEnv ->
  DynFlags ->
  UnitId ->
  FilePath ->
  StateT WorkerState IO HscEnv
loadCachedUnit logger hsc_env0 dflags0 unit file = do
  CachedUnit {build_plan, unit_args, unit_buck_args} <- liftIO $ decodeJsonBuildPlan file
  maybe (pure hsc_env0) (load build_plan unit_buck_args) unit_args
  where
    load module_graph unit_buck_args args_file =
      logTimedD logger (text "Loading cached unit" <+> quotes (ppr unit)) do
        traverse_ loadCachedArgs unit_buck_args
        hsc_env2 <- liftIO do
          args <- readFile args_file
          (dflags1, _, _, _) <- parseFlags dflags0 hsc_env0.hsc_logger (buckLocation <$> lines args)
          (hsc_env1, _) <- addHomeUnitTo hsc_env0 dflags1
          pure (hscSetActiveUnitId unit hsc_env1)
        modify (updateMakeState (insertUnitEnv hsc_env2))
        nodes <- liftIO $ traverse (uncurry (loadCachedModule hsc_env2 unit)) (Map.toList module_graph)
        modify (updateMakeState (storeModuleGraph (mkModuleGraph nodes)))
        pure hsc_env2

-- | Restore the unit state and module graph for each unit in cache that isn't present in the unit env.
loadCachedUnits ::
  Logger ->
  MVar WorkerState ->
  DynFlags ->
  CachedBuildPlans ->
  HscEnv ->
  IO HscEnv
loadCachedUnits logger stateVar dflags0 (CachedBuildPlans buildPlans) hsc_env0 =
  modifyMVar stateVar $
    logTimed logger "Loading cached units" .
    fmap swap .
    runStateT (foldM ensureBuildPlan hsc_env0 buildPlans)
  where
    ensureBuildPlan hsc_env CachedBuildPlan {name = JsonFs uid, build_plan = planFile} = do
      present <- gets \ s -> isJust (unitEnv_lookup_maybe uid s.make.hug)
      if present
      then skipPresent hsc_env uid
      else loadCachedUnit logger hsc_env dflags0 uid planFile

    skipPresent hsc_env uid = do
      logDebugD logger (text "Present in the unit env:" <+> quotes (ppr uid))
      pure hsc_env
