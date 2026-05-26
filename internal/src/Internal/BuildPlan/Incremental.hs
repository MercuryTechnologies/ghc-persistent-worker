{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}

module Internal.BuildPlan.Incremental where

import Control.Applicative ((<|>))
import Data.Aeson (encodeFile)
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import GHC (GhcMonad, ModuleName (..))
import GHC.Driver.Env (HscEnv (..), hscActiveUnitId)
import GHC.Unit.Module.Graph (ModuleGraph, mgModSummaries', mkModuleGraph, mkNodeKey)
import GHC.Utils.Outputable (SDoc)
import Internal.Cache.Metadata (loadCachedModules)
import Internal.Compat.GHC914 (hscModuleGraph)
import Internal.Json (optionalJsonFile, requiredJsonFile)
import System.OsPath.Extra (OsPath, fromOsPath)
import Types.BuckArgs (decodeJsonArg)
import Types.BuildPlan (
  BuildPlanJson (..),
  BuildPlanSchema (..),
  ModuleKey,
  PackageDeps (..),
  moduleKey,
  unionPackageDepsShallow,
  )
import Types.BuildPlan.Incremental (
  BuckHashesPath (..),
  BuildPlanPath (..),
  IncrementalState (..),
  IncrementalStatePath (..),
  SourceChanges,
  SourceHashes,
  sourceChanges,
  sourceHashesForTargets,
  storeHashes,
  )
import Types.CachedDeps (CachedModule (..), CachedUnit (..), JsonFs (..))

-- | Read the source hashes file provided by the external build tool and convert it to 'SourceHashes', discarding all
-- entries that don't correspond to the current unit's target sources.
--
-- This throws an error if the file doesn't exist.
readSourceHashes ::
  GhcMonad m =>
  Set OsPath ->
  BuckHashesPath ->
  m SourceHashes
readSourceHashes targets (BuckHashesPath path) = do
  sourceHashes <- requiredJsonFile "incremental source hashes" path
  pure (sourceHashesForTargets sourceHashes targets)

-- | Determine the changes made to the source tree since the last build.
-- Load the state persisted by the worker and compare it to both the current hashes and the
-- This is expected to be absent when no previous build has written incremental state.
loadIncrementalState ::
  GhcMonad m =>
  IncrementalStatePath ->
  SourceHashes ->
  Set OsPath ->
  m (Maybe (SourceChanges, BuildPlanJson))
loadIncrementalState incrementalState hashes targets = do
  state <- optionalJsonFile "incremental action state" incrementalState.path
  pure $ state <&> \ s -> (sourceChanges s hashes targets, s.buildPlanJson)

-- | Write the incremental state file after a successful metadata computation.
writeIncrementalState :: IncrementalStatePath -> SourceHashes -> BuildPlanJson -> IO ()
writeIncrementalState incrementalState hashes buildPlanJson =
  encodeFile (fromOsPath incrementalState.path) (storeHashes hashes buildPlanJson)

-- | Merge the result of an incremental downsweep (targeting only modified modules)
-- with the full cached module graph (containing all modules from the previous run).
--
-- Nodes from the fresh graph take precedence; cached nodes not present in the fresh
-- graph are retained.
mergeModuleGraphs :: ModuleGraph -> ModuleGraph -> ModuleGraph
mergeModuleGraphs freshGraph cachedGraph =
  mkModuleGraph (mgModSummaries' freshGraph ++ retainedCachedNodes)
  where
    freshKeys = Set.fromList (map mkNodeKey (mgModSummaries' freshGraph))
    retainedCachedNodes =
      filter (\ n -> not (mkNodeKey n `Set.member` freshKeys)) (mgModSummaries' cachedGraph)

-- | Merge two schema fields, returning an error when a field is 'Just' in one and 'Nothing' in the other operand.
-- If the first build was run with different fields than the second, the cache needs to be invalidated, since the
-- incremental update creates a partial graph that would be incomplete otherwise.
mergeField :: (a -> a -> a) -> Maybe a -> Maybe a -> Either SDoc (Maybe a)
mergeField merge = \cases
  (Just fresh) (Just cached) -> Right (Just (merge fresh cached))
  Nothing Nothing -> Right Nothing
  _ _ -> Left "This build has incremental metadata, but the previous build was run with a different set of build plan fields. Please clear the cache."

mergeBuildPlanSchema :: BuildPlanSchema -> BuildPlanSchema -> Either SDoc BuildPlanSchema
mergeBuildPlanSchema fresh cached = do
  exposed_modules <- catField fresh.exposed_modules cached.exposed_modules
  module_graph <- catField fresh.module_graph cached.module_graph
  package_deps <- mergeField unionPackageDepsShallow fresh.package_deps cached.package_deps
  project_deps <- mergeField unionPackageDepsShallow fresh.project_deps cached.project_deps
  toolchain_deps <- mergeField unionPackageDepsShallow fresh.toolchain_deps cached.toolchain_deps
  th_modules <- catField fresh.th_modules cached.th_modules
  cache <- catField fresh.cache cached.cache
  pure BuildPlanSchema {..}
  where
    catField :: Semigroup a => Maybe a -> Maybe a -> Either SDoc (Maybe a)
    catField = mergeField (<>)

-- | Merge an incremental build plan update into the cached plan used to create it.
-- Return an error if the requested fields differ.
--
-- The legacy field is discarded, because it is impossible to recover from cache, and considered incompatible with
-- incremental actions.
mergeBuildPlanJson :: BuildPlanJson -> BuildPlanJson -> Either SDoc BuildPlanJson
mergeBuildPlanJson fresh cached = do
  schema <- mergeBuildPlanSchema fresh.schema cached.schema
  pure BuildPlanJson {legacy = Nothing, schema}

-- | Remove entries from the cached unit that match the given paths.
-- These paths have been determined to be either modified or deleted by comparing hashes, so their cache entries may be
-- out of date and must be recomputed by @downsweep@.
invalidateCachedUnit :: CachedUnit -> Set OsPath -> (CachedUnit, Set ModuleName)
invalidateCachedUnit unit paths =
  maybe (unit, []) invalidate (unit.cache <|> unit.build_plan)
  where
    invalidate mods =
      let (names, pruned) = Map.partition matchPath mods
      in (unit {cache = Just pruned, build_plan = Just pruned}, Set.map coerce (Map.keysSet names))

    matchPath CachedModule {source} = Set.member source paths

loadCachedGraph ::
  Bool ->
  BuildPlanPath ->
  Set OsPath ->
  HscEnv ->
  IO (ModuleGraph, Set ModuleName)
loadCachedGraph useFixedNodes buildPlanPath invalidated hsc_env = do
  cachedUnit <- decodeJsonArg @CachedUnit "cached module graph" buildPlanPath.path
  let (cachedUnitValid, invalidatedModules) = invalidateCachedUnit cachedUnit invalidated
  graph <- loadCachedModules useFixedNodes hsc_env (hscActiveUnitId hsc_env) cachedUnitValid
  pure (graph, invalidatedModules)

-- TODO this is likely extremely inefficient when the deps graph is large, but @extendMG@ is not public.
-- We should benchmark this by inlining it.
-- TODO Merging the deps after downsweep works as well in the test, for some reason.
-- We should check whether additional, useless, nodes have an impact on performance, and see why it works.
mergeCacheAndDeps :: ModuleGraph -> HscEnv -> ModuleGraph
mergeCacheAndDeps cached hsc_env =
  mkModuleGraph (mgModSummaries' (hscModuleGraph hsc_env) ++ mgModSummaries' cached)

pruneCachedPlan :: Set ModuleName -> BuildPlanJson -> BuildPlanJson
pruneCachedPlan invalidated BuildPlanJson {schema = BuildPlanSchema {..}, ..} =
  BuildPlanJson {
    schema = BuildPlanSchema {
      exposed_modules = flip Set.difference invalidatedKeys <$> exposed_modules,
      module_graph = removeFromMap <$> module_graph,
      package_deps = removeFromDeps <$> package_deps,
      project_deps = removeFromDeps <$> project_deps,
      toolchain_deps = removeFromDeps <$> toolchain_deps,
      th_modules = flip Set.difference invalidatedKeys <$> th_modules,
      cache = removeFromMap <$> cache
    },
    ..
  }
  where
    removeFromDeps (PackageDeps m) = PackageDeps (Map.withoutKeys m invalidatedKeys)

    removeFromMap :: forall a . Map ModuleKey a -> Map ModuleKey a
    removeFromMap m = Map.withoutKeys m invalidatedKeys

    invalidatedKeys = Set.map moduleKey invalidated
