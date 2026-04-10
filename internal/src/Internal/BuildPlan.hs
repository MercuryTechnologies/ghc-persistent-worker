{-# LANGUAGE CPP, DeriveAnyClass, StrictData, OverloadedStrings #-}
#define FIXED_NODES defined(MWB_2025_10)

module Internal.BuildPlan where

#if FIXED_NODES

import GHC.Types.Error (mkUnknownDiagnostic)

#endif

import Control.Applicative ((<|>))
import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import Data.Aeson (ToJSON (..), Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.IORef (newIORef)
import Data.List.NonEmpty (NonEmpty (..), groupAllWith)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Traversable (for)
import qualified GHC
import GHC.Data.FastString (unpackFS)
import GHC.Data.Maybe (fromMaybe, mapMaybe)
import GHC.Driver.DynFlags (DynFlags (..))
import GHC.Driver.Env (HscEnv (..), hscActiveUnitId, hsc_units)
import GHC.Driver.Errors.Types (GhcMessage (GhcDriverMessage))
import GHC.Driver.Make (downsweep)
import GHC.Driver.Monad (GhcMonad (..), Session (..), liftIO, reflectGhc, withSession)
import GHC.Driver.Phases (Phase (Unlit), StopPhase (..), startPhase)
import GHC.Driver.Pipeline (TPhase (..), mkPipeEnv, runPipeline, use)
import GHC.Driver.Pipeline.Monad (PipelineOutput (..))
import GHC.Driver.Session (pgm_F)
import GHC.Generics (Generic)
import GHC.Types.Error (unionManyMessages)
import GHC.Types.SourceError (throwErrors)
import GHC.Types.Unique.Map (UniqMap, lookupUniqMap)
import GHC.Unit (UnitState (..))
import GHC.Unit.Module (
  IsBootInterface (..),
  ModLocation (..),
  ModuleName (..),
  UnitId (..),
  moduleNameString,
  unitIdString,
  )
import GHC.Unit.Module.Graph (
  ModuleGraph (..),
  ModuleGraphNode (..),
  NodeKey (..),
  mgModSummaries,
  mgModSummaries',
  mkModuleGraph,
  msKey,
  )
import GHC.Unit.Module.ModSummary (ModSummary (..), isBootSummary, msHsFilePath, ms_mod_name, ms_unitid)
import GHC.Unit.State (GenericUnitInfo (..), PackageName (..), UnitInfo)
import GHC.Utils.Error (isEmptyMessages)
import qualified System.File.OsPath as OsPath
import System.FilePath (splitExtension)
import System.OsPath (OsPath)
import Types.BuildPlan (
  BuildPlan (..),
  BuildPlanEntry (..),
  BuildPlanModule (..),
  Dep (..),
  PackageDep (..),
  Preprocessor (..),
  combineBuildPlanEntries,
  )
import Types.CachedDeps (JsonFs (..))

#if !MIN_VERSION_GLASGOW_HASKELL(9,10,0,0)

import GHC.Utils.Panic.Plain

#endif

#if !defined(MWB) && !defined(MWB_2025_10)

ms_opts :: ModSummary -> [String]
ms_opts _ = []

#endif

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
  UniqMap UnitId String ->
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
        name = fromMaybe (unitIdString unit) (lookupUniqMap unitNames unit),
        modules = [moduleNameString name | Dep {name} <- toList ds]
      }

buildPlanEntry :: ModSummary -> BuildPlanModule -> BuildPlanEntry
buildPlanEntry summary entry
  | IsBoot <- isBootSummary summary = BuildPlanEntry {regular = Nothing, boot = Just entry}
  | otherwise = BuildPlanEntry {regular = Just entry, boot = Nothing}

-- | Precomputed data used by all module entries.
data BuildPlanEnv =
  BuildPlanEnv {
    hsc_env :: HscEnv,

    -- | Preprocessor specified as a CLI arg, rather than in a module header.
    globalPreprocessor :: Preprocessor,

    -- | Canonical unit names that include Cabal sublibrary suffixes.
    unitNames :: UniqMap UnitId String,

    -- |
    localModules :: Map NodeKey (Either (JsonFs ModuleName) (JsonFs ModuleName)),
    packageModules :: Map NodeKey Dep
  }

buildPlanModule ::
  BuildPlanEnv ->
  (ModSummary, Set NodeKey) ->
  IO (JsonFs ModuleName, BuildPlanEntry)
buildPlanModule env (summary, depKeys) = do
  preprocessor <- modulePreprocessor env.hsc_env env.globalPreprocessor summary
  cpp <- cpp_deps
  let module_ = BuildPlanModule {
    sources = pure src_file,
    modules,
    modulesBoot,
    packages = modulePackageDeps env.unitNames env.packageModules depKeys,
    cpp,
    options = ms_opts summary,
    preprocessor
  }
  pure (JsonFs (ms_mod_name summary), buildPlanEntry summary module_)
  where
    src_file = msHsFilePath summary

    (modules, modulesBoot) = partitionEithers $ Map.elems $ Map.restrictKeys env.localModules depKeys

    cpp_deps
      | depIncludeCppDeps env.hsc_env.hsc_dflags
      = do
        session <- Session <$> newIORef env.hsc_env
        parsedMod <- reflectGhc (GHC.parseModule summary) session
        pure (GHC.pm_extra_src_files parsedMod)

      | otherwise
      = pure []

-- | Extract interesting nodes and tag them 'Left' if they're part of the home unit.
--
-- We're only interested in module nodes and exclude those specified on the CLI.
buildPlanNode ::
  HscEnv ->
  [ModuleName] ->
  ModuleGraphNode ->
  Maybe (Either (ModSummary, Set NodeKey) (ModSummary, Set NodeKey))
buildPlanNode hsc_env exclude = \case
#if FIXED_NODES
  ModuleNode !node_deps (ModuleNodeCompile node)
#else
  ModuleNode !node_deps node
#endif
    | elem (ms_mod_name node) exclude
    -> Nothing
    | hscActiveUnitId hsc_env == ms_unitid node
    -> Just (Left (node, Set.fromList node_deps))
    | otherwise
    -> Just (Right (node, Set.fromList node_deps))
  _ -> Nothing

indexWith :: (ModSummary -> a) -> [ModSummary] -> Map NodeKey a
indexWith f =
  Map.fromList . fmap \ summary -> (NodeKey_Module (msKey summary), f summary)

-- | Separate boot modules from regular modules.
localIndex :: [ModSummary] -> Map NodeKey (Either (JsonFs ModuleName) (JsonFs ModuleName))
localIndex =
  indexWith \ summary -> isBoot summary (JsonFs (ms_mod_name summary))
  where
    isBoot summary
      | IsBoot <- isBootSummary summary = Right
      | otherwise = Left

packageIndex :: [ModSummary] -> Map NodeKey Dep
packageIndex =
  indexWith \ summary ->
    Dep {
      name = ms_mod_name summary,
      unit = ms_unitid summary,
      boot = isBootSummary summary
    }

unitName :: UnitInfo -> String
unitName unit =
  maybe name withLibName (unitComponentName unit)
  where
    PackageName nameFS = unitPackageName unit
    name = unpackFS nameFS
    withLibName (PackageName c) = name ++ ":" ++ unpackFS c

buildPlanEnv :: HscEnv -> ModuleGraph -> (BuildPlanEnv, [(ModSummary, Set NodeKey)])
buildPlanEnv hsc_env graph =
  (env, local)
  where
    env = BuildPlanEnv {
      unitNames = unitName <$> (hsc_units hsc_env).unitInfoMap,
      localModules = localIndex (fst <$> local),
      packageModules = packageIndex (fst <$> packages),
      ..
    }

    (local, packages) = partitionEithers (mapMaybe (buildPlanNode hsc_env exclude) (mgModSummaries' graph))

    exclude = depExcludeMods hsc_env.hsc_dflags

    globalPreprocessor
      | let pp = pgm_F hsc_env.hsc_dflags
      , not (null pp)
      = Preprocessor (Just pp)
      | otherwise
      = Preprocessor Nothing

buildPlanModules :: HscEnv -> ModuleGraph -> IO (Map (JsonFs ModuleName) BuildPlanEntry)
buildPlanModules hsc_env graph =
  Map.fromListWith combineBuildPlanEntries
  <$>
  traverse (buildPlanModule env) modules
  where
    (env, modules) = buildPlanEnv hsc_env graph

buildPlanForSources :: GhcMonad m => [FilePath] -> m BuildPlan
buildPlanForSources srcs = do
  targets <- for srcs \ src -> GHC.guessTarget src Nothing Nothing
  GHC.setTargets targets
  (errs, nodes) <- withSession (liftIO . downsweepWithCache)
  let msgs = unionManyMessages errs
  unless (isEmptyMessages msgs) $ throwErrors (fmap GhcDriverMessage msgs)
  hsc_env <- getSession
  let graph = mkModuleGraph nodes
  modules <- liftIO $ buildPlanModules hsc_env graph
  pure BuildPlan {graph, modules}
  where

#if defined(DOWNSWEEP_CACHE)
    downsweepWithCache hsc_env = do
      let cachedGraph = hsc_env.hsc_mod_graph
      downsweepCompat hsc_env (mgModSummaries cachedGraph) (Just cachedGraph) [] True
#else
    downsweepWithCache hsc_env = downsweepCompat hsc_env [] [] True
#endif

#if FIXED_NODES
    downsweepCompat hsc_env = downsweep hsc_env mkUnknownDiagnostic Nothing
#else
    downsweepCompat = downsweep
#endif

writeBuildPlan :: OsPath -> BuildPlan -> IO ()
writeBuildPlan path BuildPlan {modules} =
  OsPath.writeFile path (Aeson.encode modules)
