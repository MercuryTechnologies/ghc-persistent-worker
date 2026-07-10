{-# LANGUAGE CPP, PatternSynonyms, FieldSelectors #-}

module Internal.BuildPlan where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Data.Either (partitionEithers)
import Data.Foldable (for_, toList)
import Data.List.NonEmpty (NonEmpty (..), groupAllWith)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import qualified GHC
import GHC (Target)
import GHC.Data.Maybe (mapMaybe)
import GHC.Driver.Backend (noBackend)
import GHC.Driver.DynFlags (backend)
import GHC.Driver.Env (HscEnv (..), hscActiveUnitId, hsc_units)
import GHC.Driver.Errors.Types (DriverMessages, GhcMessage (..))
import GHC.Driver.Monad (GhcMonad (..), liftIO, withSession)
import GHC.Driver.Phases (Phase (Unlit), StopPhase (..), startPhase)
import GHC.Driver.Pipeline (TPhase (..), mkPipeEnv, runPipeline, use)
import GHC.Driver.Pipeline.Monad (PipelineOutput (..))
import GHC.Driver.Session (pgm_F)
import GHC.Types.Error (unionManyMessages)
import GHC.Types.SourceError (throwErrors)
import GHC.Types.Unique.Map (UniqMap)
import GHC.Unit (GenWithIsBoot (..), UnitState (..))
import GHC.Unit.Env (UnitEnv (..))
import GHC.Unit.Home.Graph (unitEnv_keys)
import GHC.Unit.Module (IsBootInterface (..), ModLocation (..), ModuleName (..), UnitId (..))
import GHC.Unit.Module.Graph (
  ModNodeKeyWithUid (..),
  ModuleGraph,
  ModuleGraphNode (..),
  NodeKey (..),
  mgModSummaries',
  msKey,
  )
import GHC.Unit.Module.ModSummary (ModSummary (..), isBootSummary, msHsFilePath, ms_unitid)
import GHC.Utils.Error (isEmptyMessages)
import GHC.Utils.Misc (ordNub)
import GHC.Utils.Outputable (int, (<+>))
import Internal.BuildPlan.External (packageName, unitImports)
import Internal.BuildPlan.Incremental (
  loadCachedGraph,
  loadIncrementalState,
  mergeBuildPlanJson,
  mergeCacheAndDeps,
  pruneCachedPlan,
  readSourceHashes,
  writeIncrementalState,
  )
import Internal.BuildPlan.Json (assembleFields, mergePackageDeps)
import qualified Internal.Compat.FixedNodes as FixedNodes
import Internal.Compat.FixedNodes (pattern CompileNode, pattern FixedNode, downsweepCompat)
import Internal.Compat.GHC914 (edgeTarget, hscModuleGraph, mapMGM)
import Internal.Error (eitherMessages, eitherWorkerError)
import Internal.Log (logTimed)
import System.FilePath (splitExtension)
import System.OsPath.Extra (OsPath, fromOsPath, toOsPath)
import Types.Args (BuildPlanField (..))
import Types.BuildPlan (
  BuildPlan (..),
  BuildPlanEnv (..),
  BuildPlanJson (..),
  BuildPlanModule (..),
  Dep (..),
  ModuleKey,
  PackageDep (..),
  PackageKey,
  Preprocessor (..),
  emptyBuildPlanJson,
  moduleKey,
  packageKey,
  summaryModuleKey,
  )
import Types.BuildPlan.Incremental (
  BuckHashesPath,
  BuildPlanPath,
  IncrementalStatePath,
  SourceChanges (..),
  emptySourceHashes,
  )
import Types.CachedDeps (JsonFs (..))
import Types.FeatureFlags (FeatureFlags (..))
import Types.Log (Logger (..))

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

import GHC.Unit.Module.ModSummary (isTemplateHaskellOrQQNonBoot)

#else

import GHC.Unit.Module.Graph (isTemplateHaskellOrQQNonBoot)

#endif

-- TODO move ALL CPP compat stuff to dedicated modules
#if defined(DOWNSWEEP_CACHE)

import GHC.Unit.Module.Graph (mgModSummaries)

#endif

isBoot :: ModSummary -> Bool
isBoot summary = isBootSummary summary == IsBoot

modulePreprocessor :: HscEnv -> Preprocessor -> ModSummary -> IO Preprocessor
modulePreprocessor hsc_env globalPreprocessor summary
  | Just src <- ml_hs_file (ms_location summary)
  = runPipeline (hsc_hooks hsc_env) $ do
    let (_, suffix) = splitExtension src
        lit | Unlit _ <- startPhase suffix = True
            | otherwise = False
        pipe_env = mkPipeEnv StopPreprocess src Nothing NoOutputFile
    unlit_fn <- if lit then use (T_Unlit pipe_env hsc_env src) else pure src
    (dflags1, _, _) <- use (T_FileArgs hsc_env unlit_fn)
    let pp = pgm_F dflags1
    pure (if null pp then globalPreprocessor else Preprocessor (Just pp))
  | otherwise
  = pure globalPreprocessor

modulePackageDeps ::
  UniqMap UnitId PackageKey ->
  Map NodeKey Dep ->
  Set NodeKey ->
  [PackageDep]
modulePackageDeps unitNames deps keys =
  fmap packageDep $
  groupAllWith (.unit) $
  Map.elems $
  Map.restrictKeys deps keys
  where
    packageDep ds@(Dep {unit} :| _) =
      PackageDep {
        id = JsonFs unit,
        name = packageName unitNames unit,
        modules = [JsonFs name | Dep {name} <- toList ds]
      }

buildPlanModule ::
  BuildPlanEnv ->
  Map ModuleKey [String] ->
  (ModSummary, Set NodeKey) ->
  IO (ModuleKey, BuildPlanModule)
buildPlanModule env perModuleFlags (summary, depKeys) = do
  preprocessor <- modulePreprocessor env.hsc_env env.globalPreprocessor summary
  let bpModule = BuildPlanModule {
    source,
    sources = [source],
    boot = isBoot summary,
    modules,
    modulesBoot,
    packages = modulePackageDeps env.unitNames env.packageModules depKeys,
    thEnabled = isTemplateHaskellOrQQNonBoot summary,
    preprocessor,
    flags
  }
  pure (summaryModuleKey summary, bpModule)
  where
    source = toOsPath (msHsFilePath summary)
    flags = Map.findWithDefault [] (summaryModuleKey summary) perModuleFlags

    (modules, modulesBoot) = partitionEithers $ Map.elems $ Map.restrictKeys env.homeModules depKeys

-- | Classify a module graph node for the build plan.
--
-- Module graphs contain several different node types.
-- The build plan's purpose is to provide dependency information between the project's modules to the external build
-- tool, so we're only interested in nodes that represent modules.
--
-- There are two subtypes of module node:
-- - Compile nodes, which represent modules that are intended to be compiled in a later action.
--   These contain a 'ModSummary' with the parsed AST and other data required for compilation.
-- - Fixed nodes, representing modules that are already built.
--   These only contain a 'ModLocation', through which their interface files may be located while compiling dependents.
--
-- Both need to be included in the build plan, since the build tool processes the dependencies of the entire unit even
-- when only parts of it have changed, and we don't want any inconsistencies between the two builds.
--
-- The results of this function are used to construct memory-efficient lookup indexes, which is why the return avoids
-- new, more expressive types:
--
-- - 'Nothing' is a node that's of no use to the build tool: Backpack nodes, unit nodes and link nodes.
--
-- - 'Left' is a module belonging to the build plan (home) unit.
--   If this module is scheduled to be compiled, the node will contain the full 'ModSummary' as well as its
--   dependencies.
--   If it was restored from incremental cache as a fixed node, it will only provide its unit ID and module name.
--
-- - 'Right' is a module from a different unit, which is only required to resolve dependencies from the home unit, so
--   its unit ID and module name are sufficient for the build plan.
--
-- The data is shaped like it is because it allows 'buildPlanEnv' to partition the entirety of the modules.
buildPlanNode ::
  HscEnv ->
  ModuleGraphNode ->
  Maybe (Either (ModNodeKeyWithUid, Maybe (ModSummary, Set NodeKey)) ModNodeKeyWithUid)
buildPlanNode hsc_env = \case
  CompileNode {depsCompile, summary}
    | hscActiveUnitId hsc_env == ms_unitid summary
    -> Just (Left (msKey summary, Just (summary, Set.fromList (edgeTarget <$> depsCompile))))
    | otherwise
    -> Just (Right (msKey summary))
  FixedNode {key}
    | hscActiveUnitId hsc_env == key.mnkUnitId
    -> Just (Left (key, Nothing))
    | otherwise
    -> Just (Right key)
  _ -> Nothing

-- | Convert a list of module metadata to a 'Map' using a value constructor function.
indexWith ::
  (ModNodeKeyWithUid -> a) ->
  [ModNodeKeyWithUid] ->
  Map NodeKey a
indexWith f =
  Map.fromList . fmap \ key -> (NodeKey_Module key, f key)

-- | Create a lookup index for all modules in the build plan home unit that allows sharing constructor closures for
-- memory efficiency.
-- This is indexed later with the 'NodeKey's from each module's dependency set.
--
-- For easier partioning, boot modules are wrapped in 'Right' and regular modules in 'Left'.
-- If 'indexWith' passes 'Nothing' for the 'ModSummary' argument to the callback, we're dealing with a fixed node.
localIndex :: [ModNodeKeyWithUid] -> Map NodeKey (Either (ModuleKey, JsonFs ModuleName) (ModuleKey, JsonFs ModuleName))
localIndex =
  indexWith \ ModNodeKeyWithUid {mnkModuleName = GWIB {gwib_mod, gwib_isBoot}} ->
    classify gwib_isBoot (moduleKey gwib_mod, JsonFs gwib_mod)
  where
    classify = \case
      IsBoot -> Right
      NotBoot -> Left

-- | Create a lookup index for modules in non-home units that allows sharing constructor closures for memory efficiency.
packageIndex :: [ModNodeKeyWithUid] -> Map NodeKey Dep
packageIndex =
  indexWith \ ModNodeKeyWithUid {mnkModuleName = GWIB {gwib_mod = name}, mnkUnitId = unit} ->
    Dep {name, unit}

-- | Precompute lookup indexes and module metadata for the build plan.
--
-- JSON data structures for a module graph with thousands of modules and tens of thousands of dependencies can cause
-- significant memory overhead when each dependency allocates a fresh constructor closure, or even a string, if that
-- dependency is present in many modules.
--
-- To mitigate this, we create indexes of the data structures that are most commonly shared, like 'ModuleKey' and
-- 'PackageKey', so that they can be looked up and reused rather than recreated from the GHC types in 'ModuleGraph'.
--
-- The data for each module is constructed in 'buildPlanNode', which is then fed into 'localIndex' and 'packageIndex' to
-- create the lookup indexes.
-- In 'buildPlanModule', these indexes are then queried to replace the 'NodeKey' values from the 'ModuleGraph' with
-- those shared closures.
--
-- Aside from the 'BuildPlanEnv', this also returns the full set of nodes belonging to the home unit, as 'ModSummary'
-- and set of dependency 'NodeKey's.
buildPlanEnv ::
  HscEnv ->
  ModuleGraph ->
  (BuildPlanEnv, [(ModSummary, Set NodeKey)])
buildPlanEnv hsc_env graph =
  (env, updated)
  where
    env = BuildPlanEnv {
      unitNames,
      homeUnitIds,
      homeModules = localIndex (fst <$> local),
      packageModules = packageIndex packages,
      ..
    }

    unitNames = packageKey <$> (hsc_units hsc_env).unitInfoMap

    homeUnitIds = unitEnv_keys hsc_env.hsc_unit_env.ue_home_unit_graph

    updated = mapMaybe snd local

    (local, packages) = partitionEithers (mapMaybe (buildPlanNode hsc_env) (mgModSummaries' graph))

    globalPreprocessor
      | null preprocessorFlag
      = Preprocessor Nothing
      | otherwise
      = Preprocessor (Just preprocessorFlag)

    preprocessorFlag = pgm_F hsc_env.hsc_dflags

-- | Compute lookup indexes for a module graph and construct a JSON build plan payload for an external build tool for
-- all modules in the active home unit.
-- Look up all imports of modules that aren't present in the graph in the external package databases.
buildPlanModules ::
  GhcMonad m =>
  Set BuildPlanField ->
  Map ModuleKey [String] ->
  Set UnitId ->
  ModuleGraph ->
  HscEnv ->
  m BuildPlanJson
buildPlanModules fields perModuleFlags staticUnits graph hsc_env = do
  externalDeps <-
    if includeExternalDeps
    then eitherMessages GhcDriverMessage =<< liftIO (unitImports env (fst <$> modules))
    else pure []
  let parts = Map.partitionWithKey (\ unit _ -> Set.member unit staticUnits) <$> externalDeps
      staticDeps = fmap (fmap ordNub) . fst <$> parts
      toolchainDeps = snd <$> parts
  bpModules <- Map.fromList <$> liftIO (traverse (buildPlanModule env perModuleFlags) modules)
  pure (assembleFields fields toolchainDeps (mergePackageDeps staticDeps bpModules))
  where
    (env, modules) = buildPlanEnv hsc_env graph

    includeExternalDeps =
      FieldToolchainDeps `elem` fields || FieldPackageDeps `elem` fields || not (Set.null staticUnits)

downsweepWithCache :: ModuleGraph -> HscEnv -> IO ([DriverMessages], ModuleGraph)

#if defined(DOWNSWEEP_CACHE)

downsweepWithCache cache hsc_env =
  downsweepCompat hsc_env (mgModSummaries cache) (Just cache) [] True

#else

downsweepWithCache _ hsc_env = downsweepCompat hsc_env [] Nothing [] True

#endif

-- | Disabling the backend, in conjunction with setting `ghcMode = MkDepend`, prevents
--   downsweep from performing TH dependency analysis, which is the external build tool's
--   responsibility.
useNoBackend :: HscEnv -> HscEnv
useNoBackend hsc_env =
  let dflags = hsc_dflags hsc_env
   in hsc_env { hsc_dflags = dflags {backend = noBackend}}

-- | Add the per-module flags to each module's 'ms_hspp_opts' in the module graph.
addPerModuleFlagsToModuleGraph
  :: GhcMonad m => Map.Map ModuleKey [String] -> ModuleGraph -> m ModuleGraph
addPerModuleFlagsToModuleGraph perModuleFlags mg0 =
  if Map.null perModuleFlags then
    pure mg0
  else do
    hsc_env <- getSession
    liftIO $ flip mapMGM mg0 $ \summary ->
      case Map.lookup (summaryModuleKey summary) perModuleFlags of
        Just flags -> do
          (dflags, _, _) <- GHC.parseDynamicFlags
             hsc_env.hsc_logger
             (GHC.ms_hspp_opts summary)
             (map GHC.noLoc flags)
          pure summary {GHC.ms_hspp_opts = dflags}
        _ -> pure summary

checkErrors :: MonadIO m => [DriverMessages] -> m ()
checkErrors errs =
  unless (isEmptyMessages msgs) do
    throwErrors (GhcDriverMessage <$> msgs)
  where
    msgs = unionManyMessages errs

timedWithSession ::
  GhcMonad m =>
  Logger ->
  String ->
  (HscEnv -> m a) ->
  m a
timedWithSession logger desc f =
  logTimed logger desc $ withSession f

guessFileTargets :: GhcMonad m => [OsPath] -> m [Target]
guessFileTargets =
  traverse \ src -> GHC.guessTarget (fromOsPath src) Nothing Nothing

-- | Run downsweep only for the given changed modules.
-- Unchanged modules in the home unit are provided as cached graph nodes.
-- These include both the home unit and all unit deps.
downsweepTargets ::
  GhcMonad m =>
  ModuleGraph ->
  [Target] ->
  m ModuleGraph
downsweepTargets cache targets = do
  GHC.setTargets targets
  (errs, graph) <- withSession (liftIO . downsweepWithCache cache . useNoBackend)
  checkErrors errs
  pure graph

buildPlanForTargets ::
  GhcMonad m =>
  Logger ->
  Set BuildPlanField ->
  Map ModuleKey [String] ->
  Set UnitId ->
  [Target] ->
  m BuildPlan
buildPlanForTargets logger fields perModuleFlags staticUnits targets = do
  graph0 <- timedWithSession logger "Downsweep" \ hsc_env ->
    downsweepTargets (hscModuleGraph hsc_env) targets
  graph <- addPerModuleFlagsToModuleGraph perModuleFlags graph0
  json <- timedWithSession logger "Build plan modules" $ buildPlanModules fields perModuleFlags staticUnits graph
  pure BuildPlan {graph, json}

-- | Full downsweep targeting all sources.
buildPlanFull ::
  GhcMonad m =>
  Logger ->
  Set BuildPlanField ->
  Map ModuleKey [String] ->
  Set UnitId ->
  [OsPath] ->
  m BuildPlan
buildPlanFull logger fields perModuleFlags staticUnits srcs = do
  targets <- guessFileTargets srcs
  buildPlanForTargets logger fields perModuleFlags staticUnits targets

downsweepIncremental ::
  GhcMonad m =>
  ModuleGraph ->
  [OsPath] ->
  m ModuleGraph
downsweepIncremental cache = \case
  [] -> pure cache
  paths -> do
    targets <- guessFileTargets paths
    downsweepTargets cache targets

buildPlanIncremental ::
  forall m .
  GhcMonad m =>
  Bool ->
  Logger ->
  Set BuildPlanField ->
  Map ModuleKey [String] ->
  Set UnitId ->
  BuildPlanPath ->
  SourceChanges ->
  BuildPlanJson ->
  m BuildPlan
buildPlanIncremental useFixedNodes logger fields perModuleFlags staticUnits buildPlanPath SourceChanges {updated, invalidated} cachedJson = do
  liftIO $ logger.debugD ("Incremental metadata:" <+> int (length invalidated) <+> "changed sources")
  (cached, removed) <- timed "Load cached graph" (liftIO . loadCachedGraph useFixedNodes buildPlanPath invalidated)
  valid <- timed "Merge cached graph with deps" (pure . mergeCacheAndDeps cached)
  graph <- logTimed logger "Downsweep" $ downsweepIncremental valid (Set.toList updated)
  freshJson <- timed "Build plan modules" (buildPlanModules fields perModuleFlags staticUnits graph)
  json <- logTimed logger "merge" (eitherWorkerError (mergeBuildPlanJson freshJson (pruneCachedPlan removed cachedJson)))
  pure BuildPlan {graph, json}
  where
    timed :: String -> (HscEnv -> m a) -> m a
    timed = timedWithSession logger

buildPlanForSources ::
  GhcMonad m =>
  FeatureFlags ->
  Logger ->
  Set BuildPlanField ->
  Map ModuleKey [String] ->
  Set UnitId ->
  BuildPlanPath ->
  Maybe IncrementalStatePath ->
  Maybe BuckHashesPath ->
  [OsPath] ->
  m BuildPlan
buildPlanForSources features logger fields perModuleFlags staticUnits buildPlanPath incrementalArg buckHashesPath targets
  | features.incrementalBuildPlan
  , Just incrementalStatePath <- incrementalArg
  = maybe full (withHashes incrementalStatePath) =<< traverse (readSourceHashes tset) buckHashesPath
  | otherwise
  = do
    for_ incrementalArg \ incrementalStatePath ->
      liftIO $ writeIncrementalState incrementalStatePath emptySourceHashes emptyBuildPlanJson
    full
  where
    withHashes incrementalState sourceHashes = do
      plan <- maybe full incremental =<< loadIncrementalState incrementalState sourceHashes tset
      liftIO $ writeIncrementalState incrementalState sourceHashes plan.json
      pure plan

    incremental (changes, cachedJson) =
      buildPlanIncremental features.fixedNodesCache logger fields perModuleFlags staticUnits buildPlanPath changes cachedJson

    full = buildPlanFull logger fields perModuleFlags staticUnits targets

    tset = Set.fromList targets
