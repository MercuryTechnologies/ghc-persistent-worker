{-# LANGUAGE CPP #-}

module Internal.Cache.Metadata where

import Control.Applicative ((<|>))
import Control.Concurrent (MVar, modifyMVar)
import Control.Concurrent.Async (forConcurrently)
import Control.Exception (throwIO)
import Control.Monad (foldM, (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (StateT (..), gets, modify, modifyM)
import Data.Aeson (eitherDecodeFileStrict')
import Data.Foldable (fold, traverse_)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Tuple (swap)
import qualified GHC
import GHC (DynFlags (..), IsBootInterface (..), mkModuleGraph, ModuleName)
import GHC.Driver.Env (HscEnv (..), hscSetActiveUnitId)
import GHC.Driver.Make (ModNodeKeyWithUid (..))
import GHC.Driver.Session (updatePlatformConstants)
import GHC.Unit (GenWithIsBoot (..), HomeUnit, UnitDatabase, UnitId (..), UnitState, initUnits)
import GHC.Unit.Env (HomeUnitEnv (..), UnitEnv (..), updateHug)
import GHC.Unit.Home (GenHomeUnit (DefiniteHomeUnit))
import GHC.Unit.Module.Graph (ModuleGraphNode (..), NodeKey (..))
import GHC.Utils.Outputable (ppr, quotes, text, (<+>))
import Internal.DynFlags (buckLocation, parseFlags, setupPath)
import Internal.Log (logTimed, logTimedD)
import Internal.State (updateMakeState)
import qualified Internal.State.Make as Make
import Internal.State.Make (insertUnitEnv, storeModuleGraph)
import Internal.UnitEnv (emptyHomePackageTable)
import Types.BuckArgs (CachedBuckArgs (..), parseCachedBuckArgs)
import Types.CachedDeps (
  CachedBuildPlan (..),
  CachedBuildPlans (..),
  CachedModule (..),
  CachedPackageDep (..),
  CachedUnit (..),
  JsonFs (..),
  )
import Types.Log (Logger (..))
import Types.State (WorkerState (..))
import Types.State.Make (MakeState (..))
import Internal.Compat.GHC914 (moduleNodeEdge)

#if defined(FIXED_NODES) || defined(MWB)

import GHC.Unit.Home.Graph (unitEnv_insert, unitEnv_keys, unitEnv_lookup_maybe)

#else

import GHC.Unit.Env (unitEnv_insert, unitEnv_keys, unitEnv_lookup_maybe)

#endif

#if defined(FIXED_NODES)

import GHC.Driver.Config.Finder (initFinderOpts)
import GHC.Types.SourceFile (HscSource (HsSrcFile))
import GHC.Unit.Finder (addHomeModuleToFinder, mkHomeModLocation)
import GHC.Unit.Module.Graph (ModuleNodeInfo (..))
import System.OsPath.Extra (splitExtension, toOsPath)

#else

import GHC.Driver.Errors.Types (GhcMessage (..))
import GHC.Driver.Make (summariseFile)
import Internal.Error (eitherMessages)
import System.OsPath.Extra (fromOsPath)

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
#if defined(UNIT_INDEX)
  (dbs, unit_state, home_unit, mconstants) <- initUnits logger dflags0 unit_env.ue_index Nothing allUnitIds
#else
  (dbs, unit_state, home_unit, mconstants) <- initUnits logger dflags0 Nothing allUnitIds
#endif
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

-- | Construct a 'ModuleGraphNode' from data obtained from the Buck cache and add its location to the @Finder@.
--
-- If GHC has fixed module graph nodes, those are constructed; otherwise we have to call 'summariseFile' to create a
-- full node, which parses the module.
loadCachedModule :: HscEnv -> UnitId -> JsonFs ModuleName -> CachedModule -> IO ModuleGraphNode
loadCachedModule hsc_env unit (JsonFs modName) CachedModule {source, modules, packages} = do
  node <- createNode source modName
  pure (ModuleNode (moduleNodeEdge <$> (homeDeps ++ packageDeps)) node)
  where
    homeDeps =
      [
        NodeKey_Module (ModNodeKeyWithUid (GWIB depName NotBoot) unit)
        |
        JsonFs depName <- modules
      ]

    packageDeps =
      [
        NodeKey_Module (ModNodeKeyWithUid (GWIB depName NotBoot) depUnit)
        |
        CachedPackageDep {id = JsonFs depUnit, modules = depModules} <- packages,
        JsonFs depName <- depModules
      ]

#if defined(FIXED_NODES)

    createNode src name = do
      _ <- addHomeModuleToFinder hsc_env.hsc_FC (DefiniteHomeUnit unit Nothing) name location HsSrcFile
      pure $ ModuleNodeFixed (ModNodeKeyWithUid (GWIB name NotBoot) unit) location
      where
        fopts = initFinderOpts (hsc_dflags hsc_env)
        (basename, extension) = splitExtension src
        location = mkHomeModLocation fopts name (toOsPath basename) (toOsPath extension) HsSrcFile

#else

    createNode src _ = do
      summResult <- summariseFile hsc_env (DefiniteHomeUnit unit Nothing) mempty (fromOsPath src) Nothing Nothing
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

-- | Cached CLI args for a unit.
--
-- This function is separated so that we can parallelize CLI arg parsing part.
readParseGHCArgs ::
  HscEnv ->
  DynFlags ->
  FilePath ->
  IO DynFlags
readParseGHCArgs hsc_env0 dflags0 args_file = do
  args <- readFile args_file
  (dflags1, _, _, _) <- parseFlags dflags0 hsc_env0.hsc_logger (buckLocation <$> lines args)
  pure dflags1

-- | Restore the unit state and module graph from the external cache.
--
-- The cached data consists of a simple list of GHC command line arguments that can recreate the unit state, as well as
-- the module graph produced by a previous metadata request.
loadCachedUnit ::
  Logger ->
  HscEnv ->
  UnitId ->
  (CachedUnit, DynFlags) ->
  StateT WorkerState IO HscEnv
loadCachedUnit logger hsc_env0 unit (CachedUnit {build_plan, cache, unit_buck_args}, dflags) =
  logTimedD logger (text "Loading cached unit" <+> quotes (ppr unit)) do
    traverse_ loadCachedArgs unit_buck_args
    hsc_env2 <- liftIO do
      (hsc_env1, _) <- addHomeUnitTo hsc_env0 dflags
      pure (hscSetActiveUnitId unit hsc_env1)
    modify (updateMakeState (insertUnitEnv hsc_env2))
    nodes <- liftIO $ traverse (uncurry (loadCachedModule hsc_env2 unit)) (Map.toList (fold (cache <|> build_plan)))
    modify (updateMakeState (storeModuleGraph (mkModuleGraph nodes)))
    pure hsc_env2

-- | Restore the unit state and module graph for each unit in cache that isn't present in the unit env.
--
-- Restore the unit env from state because 'initUnits' looks up dependencies.
--
-- TODO Check if the loader state needs to be restored too – it might be referenced in a closure?
-- Simple memory comparison should do it.
loadCachedUnits ::
  Logger ->
  MVar WorkerState ->
  DynFlags ->
  CachedBuildPlans ->
  HscEnv ->
  IO HscEnv
loadCachedUnits logger stateVar dflags0 (CachedBuildPlans buildPlans) hsc_env0 = do
  modifyMVar stateVar \ state -> do
    let hsc_env1 = Make.loadState hsc_env0 state.make
    logTimed logger "Loading cached units" $ fmap swap do
      buildPlans_with_cunit_and_dflags <-
        forConcurrently buildPlans \plan@CachedBuildPlan {name = JsonFs uid, build_plan = planFile} -> do
          let present = isJust (unitEnv_lookup_maybe uid state.make.hug)
          if present
            then pure (plan, Nothing)
            else do
              cachedUnit@CachedUnit {unit_args} <- liftIO $ decodeJsonBuildPlan planFile
              mdflags1 <- traverse (readParseGHCArgs hsc_env1 dflags0) unit_args
              pure (plan, (cachedUnit,) <$> mdflags1)
      runStateT (foldM ensureBuildPlan hsc_env1 buildPlans_with_cunit_and_dflags) state
  where
    ensureBuildPlan hsc_env (CachedBuildPlan {name = JsonFs uid}, mb_cachedUnit_dflags) = do
      present <- gets \ s -> isJust (unitEnv_lookup_maybe uid s.make.hug)
      if present
      then pure hsc_env
      else do
        case mb_cachedUnit_dflags of
          Nothing -> pure hsc_env -- don't we yield error here?
          Just (cachedUnit, dflags) -> do
            loadCachedUnit logger hsc_env uid (cachedUnit, dflags)
