{-# LANGUAGE CPP #-}

#define FIXED_NODES defined(MWB_2025_10)

module Internal.BuildPlan where

#if FIXED_NODES

import GHC.Types.Error (mkUnknownDiagnostic)

#endif

import Control.Monad (unless)
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..), groupAllWith)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Traversable (for)
import qualified GHC
import GHC (Target)
import GHC.Data.Maybe (mapMaybe)
import GHC.Driver.Env (HscEnv (..), hscActiveUnitId, hsc_units)
import GHC.Driver.Errors.Types (DriverMessages, GhcMessage (GhcDriverMessage))
import GHC.Driver.Make (downsweep)
import GHC.Driver.Monad (GhcMonad (..), liftIO, withSession)
import GHC.Driver.Phases (Phase (Unlit), StopPhase (..), startPhase)
import GHC.Driver.Pipeline (TPhase (..), mkPipeEnv, runPipeline, use)
import GHC.Driver.Pipeline.Monad (PipelineOutput (..))
import GHC.Driver.Session (pgm_F)
import GHC.Types.Error (unionManyMessages)
import GHC.Types.SourceError (throwErrors)
import GHC.Types.Unique.Map (UniqMap)
import GHC.Unit (UnitState (..))
import GHC.Unit.Env (UnitEnv (..))
import GHC.Unit.Module (IsBootInterface (..), ModLocation (..), ModuleName (..), UnitId (..))
import GHC.Unit.Module.Graph (ModuleGraph, ModuleGraphNode (..), NodeKey (..), mgModSummaries', msKey)
import GHC.Unit.Module.ModSummary (ModSummary (..), isBootSummary, msHsFilePath, ms_mod_name, ms_unitid)
import GHC.Utils.Error (isEmptyMessages)
import Internal.BuildPlan.External (packageName, unitImports)
import Internal.BuildPlan.Json (assembleFields)
import System.FilePath (splitExtension)
import Types.Args (BuildPlanField (..))
import Types.BuildPlan (
  BuildPlan (..),
  BuildPlanEnv (..),
  BuildPlanModule (..),
  BuildPlanJson (..),
  Dep (..),
  ModuleKey,
  PackageDep (..),
  PackageKey,
  Preprocessor (..),
  packageKey,
  summaryModuleKey,
  )
import Types.CachedDeps (JsonFs (..))

#if !MIN_VERSION_GLASGOW_HASKELL(9,10,0,0)

import GHC.Utils.Panic.Plain

#endif

#if defined(MWB) || defined(MWB_2025_10)

import GHC.Unit.Home.Graph (unitEnv_keys)

#else

import GHC.Unit.Env (unitEnv_keys)

#endif

#if defined(MWB_2025_10)

import GHC.Unit.Module.ModSummary (isTemplateHaskellOrQQNonBoot)

#else

import GHC.Unit.Module.Graph (isTemplateHaskellOrQQNonBoot)

#endif

#if FIXED_NODES

import GHC.Unit.Module.Graph (ModuleNodeInfo (..))

#else

import GHC.Unit.Module.Graph (mkModuleGraph)

#if defined(MWB) || defined(MWB_2025_10)

import GHC.Unit.Module.Graph (mgModSummaries)

#endif

#endif

#if !defined(MWB) && !defined(MWB_2025_10)

ms_opts :: ModSummary -> [String]
ms_opts _ = []

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
  (ModSummary, Set NodeKey) ->
  IO (ModuleKey, BuildPlanModule)
buildPlanModule env (summary, depKeys) = do
  preprocessor <- modulePreprocessor env.hsc_env env.globalPreprocessor summary
  let bpModule = BuildPlanModule {
    source,
    sources = [source],
    boot = isBoot summary,
    modules,
    modulesBoot,
    packages = modulePackageDeps env.unitNames env.packageModules depKeys,
    options = Set.fromList (ms_opts summary),
    thEnabled = isTemplateHaskellOrQQNonBoot summary,
    preprocessor
  }
  pure (summaryModuleKey summary, bpModule)
  where
    source = msHsFilePath summary

    (modules, modulesBoot) = partitionEithers $ Map.elems $ Map.restrictKeys env.homeModules depKeys

-- | Extract interesting nodes and tag them 'Left' if they're part of the home unit.
--
-- We're only interested in module nodes.
buildPlanNode ::
  HscEnv ->
  ModuleGraphNode ->
  Maybe (Either (ModSummary, Set NodeKey) (ModSummary, Set NodeKey))
buildPlanNode hsc_env = \case
#if FIXED_NODES
  ModuleNode !node_deps (ModuleNodeCompile node)
#else
  ModuleNode !node_deps node
#endif
    | hscActiveUnitId hsc_env == ms_unitid node
    -> Just (Left (node, Set.fromList node_deps))
    | otherwise
    -> Just (Right (node, Set.fromList node_deps))
  _ -> Nothing

indexWith :: (ModSummary -> a) -> [ModSummary] -> Map NodeKey a
indexWith f =
  Map.fromList . fmap \ summary -> (NodeKey_Module (msKey summary), f summary)

-- | Separate boot modules from regular modules.
localIndex :: [ModSummary] -> Map NodeKey (Either (ModuleKey, JsonFs ModuleName) (ModuleKey, JsonFs ModuleName))
localIndex =
  indexWith \ summary -> decide summary (summaryModuleKey summary, JsonFs (ms_mod_name summary))
  where
    decide summary = if isBoot summary then Right else Left

packageIndex :: [ModSummary] -> Map NodeKey Dep
packageIndex =
  indexWith \ summary ->
    Dep {
      name = ms_mod_name summary,
      unit = ms_unitid summary
    }

buildPlanEnv ::
  HscEnv ->
  ModuleGraph ->
  (BuildPlanEnv, [(ModSummary, Set NodeKey)])
buildPlanEnv hsc_env graph =
  (env, local)
  where
    env = BuildPlanEnv {
      unitNames,
      homeUnitIds,
      homeModules = localIndex (fst <$> local),
      packageModules = packageIndex (fst <$> packages),
      ..
    }

    unitNames = packageKey <$> (hsc_units hsc_env).unitInfoMap

    homeUnitIds = unitEnv_keys hsc_env.hsc_unit_env.ue_home_unit_graph

    (local, packages) = partitionEithers (mapMaybe (buildPlanNode hsc_env) (mgModSummaries' graph))

    globalPreprocessor
      | let pp = pgm_F hsc_env.hsc_dflags
      , not (null pp)
      = Preprocessor (Just pp)
      | otherwise
      = Preprocessor Nothing

buildPlanModules ::
  Set BuildPlanField ->
  HscEnv ->
  ModuleGraph ->
  IO BuildPlanJson
buildPlanModules fields hsc_env graph = do
  toolchainDeps <-
    if includeToolchainDeps
    then unitImports env (fst <$> modules)
    else mempty
  assembleFields fields toolchainDeps . Map.fromList <$> traverse (buildPlanModule env) modules
  where
    (env, modules) = buildPlanEnv hsc_env graph

    includeToolchainDeps = FieldToolchainDeps `elem` fields || FieldPackageDeps `elem` fields

downsweepCompat ::
  HscEnv ->
  [ModSummary] ->
  Maybe ModuleGraph ->
  [ModuleName] ->
  Bool ->
  IO ([DriverMessages], ModuleGraph)

#if FIXED_NODES

downsweepCompat hsc_env summaries _ =
  downsweep hsc_env mkUnknownDiagnostic Nothing summaries

#elif defined(MWB)

downsweepCompat hsc_env summaries cache excl dup =
  fmap mkModuleGraph <$> downsweep hsc_env summaries cache excl dup

#else

downsweepCompat hsc_env summaries _ excl dup =
  fmap mkModuleGraph <$> downsweep hsc_env summaries excl dup

#endif

downsweepWithCache :: HscEnv -> IO ([DriverMessages], ModuleGraph)

#if defined(DOWNSWEEP_CACHE)

downsweepWithCache hsc_env = do
  let cachedGraph = hsc_env.hsc_mod_graph
  downsweepCompat hsc_env (mgModSummaries cachedGraph) (Just cachedGraph) [] True

#else

downsweepWithCache hsc_env = downsweepCompat hsc_env [] Nothing [] True

#endif

buildPlanForTargets ::
  GhcMonad m =>
  Set BuildPlanField ->
  [Target] ->
  m BuildPlan
buildPlanForTargets fields targets = do
  GHC.setTargets targets
  (errs, graph) <- withSession (liftIO . downsweepWithCache)
  let msgs = unionManyMessages errs
  unless (isEmptyMessages msgs) $ throwErrors (fmap GhcDriverMessage msgs)
  hsc_env <- getSession
  json <- liftIO $ buildPlanModules fields hsc_env graph
  pure BuildPlan {graph, json}

buildPlanForSources ::
  GhcMonad m =>
  Set BuildPlanField ->
  [FilePath] ->
  m BuildPlan
buildPlanForSources fields srcs = do
  targets <- for srcs \ src -> GHC.guessTarget src Nothing Nothing
  buildPlanForTargets fields targets
