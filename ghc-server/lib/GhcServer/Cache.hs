-- | Cache logic for the standalone GHC server.
--
-- After a successful metadata step, cache data is written to @cache/@ so that subsequent builds can restore the
-- 'WorkerState' (HUG, module graph) without rerunning metadata from scratch.
--
-- The cache format mirrors what Buck writes for the persistent worker:
--
-- - Per unit: a 'CachedUnit' JSON with the module graph cache and a @unit_args@ file with GHC CLI flags.
--
-- On restore, 'loadCachedUnits' rebuilds the HUG and module graph from the cached data.
-- @Opt_ForceRecomp@ disabled, so GHC's native recompilation checking skips modules whose @.hi@ files are up to date.
module GhcServer.Cache where

import Control.Monad (filterM, foldM)
import Control.Monad.Extra (whenMaybeM)
import qualified Data.Aeson as Aeson
import Data.Aeson (eitherDecodeFileStrict')
import Data.ByteString (fromStrict)
import qualified Data.ByteString.Char8 as B8
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import GHC (ModuleName)
import GHC.Data.Graph.Directed (Graph, reachablesG, topologicalSortG)
import qualified GHC.Data.Graph.Directed as Graph (Node (..))
import GHC.Unit (stringToUnit)
import GHC.Unit.Types (toUnitId)
import GhcServer.Data.BuildCache (BuildCache (..))
import GhcServer.Data.Unit (Project (..), Unit (..), UnitCache (..), UnitDepNode, UnitName (..), moduleHiPath)
import GhcServer.Path (fp, osPath)
import qualified System.Directory.OsPath as OsPath
import System.Directory.OsPath (createDirectoryIfMissing, doesFileExist)
import qualified System.File.OsPath as OsPath
import System.OsPath (OsPath, (</>))
import System.OsPath.Extra (fromOsPath)
import Types.BuildPlan.Incremental (BuildPlanPath (..))
import Types.CachedDeps (CachedBuildPlan (..), CachedBuildPlans (..), CachedUnit (..), JsonFs (..))
import Types.Log (Logger (..))

-- | Write the @dep_units.json@ file and return its path, if there are dep plans to write.
writeDepUnits :: UnitCache -> Maybe CachedBuildPlans -> IO (Maybe OsPath)
writeDepUnits unitCache =
  traverse \ plans -> do
    Aeson.encodeFile (fromOsPath unitCache.depUnitsPath) plans
    pure unitCache.depUnitsPath

-- | Decode the build plan JSON and write @cached_unit.json@ with injected @unit_args@ and @dep_units@ paths.
writeCachedUnit :: UnitCache -> Maybe OsPath -> FilePath -> IO (Either String ())
writeCachedUnit unitCache depsFile buildPlanFp =
  eitherDecodeFileStrict' buildPlanFp >>= \case
    Left err ->
      pure (Left ("Failed to decode build plan for cache (" ++ buildPlanFp ++ "): " ++ err))
    Right cachedUnit -> do
      OsPath.writeFile (unitCache.dir </> osPath "cached_unit.json") (Aeson.encode cachedUnit {
        unit_args = Just unitCache.unitArgsPath,
        dep_units = depsFile
      })
      pure (Right ())

-- | Write the cache files for a unit after a successful metadata step.
--
-- Writes:
--
-- 1. @unit_args@ GHC CLI flags, one per line.
-- 2. @dep_units.json@ 'CachedBuildPlans' for the unit's transitive dep units (from the pre-computed graph query).
-- 3. @cached_unit.json@ 'CachedUnit' with the @cache@ field from the build plan JSON, the @unit_args@ path, and
--    the @dep_units@ path.
--
-- The 'CachedUnit' is constructed by decoding the build plan JSON that 'computeMetadata' wrote (which contains a
-- @cache@ field compatible with 'CachedUnit'), then setting the @unit_args@ and @dep_units@ paths.
writeUnitCache :: Logger -> UnitCache -> Maybe CachedBuildPlans -> BuildPlanPath -> [String] -> IO (Either String ())
writeUnitCache _logger unitCache depPlans buildPlanPath ghcOptions =
  doesFileExist buildPlanPath.path >>= \case
    False -> pure (Right ())
    True -> writeAll
  where
    buildPlanFp = fp buildPlanPath.path

    writeAll = do
      createDirectoryIfMissing True unitCache.dir
      OsPath.writeFile unitCache.unitArgsPath (fromStrict (B8.unlines (map B8.pack ghcOptions)))
      depsFile <- writeDepUnits unitCache depPlans
      writeCachedUnit unitCache depsFile buildPlanFp

-- | Check whether a cache exists for a unit.
cacheExists :: UnitCache -> IO Bool
cacheExists unitCache =
  doesFileExist unitCache.cachedUnitPath

-- | Order the transitive dependencies of a unit for loading by 'loadCachedUnits'.
--
-- Returns nodes in dependency order (leaves first): each unit appears after all
-- units it depends on.  This ordering is required because 'loadCachedUnits'
-- processes plans sequentially and each unit's 'initUnits' call expects all of
-- its @-package-id@ targets to already be in the home unit graph.
--
-- 'reachablesG' alone is insufficient: it returns nodes in DFS pre-order
-- (roots first), and reversing that is still wrong for DAGs with shared
-- ancestors — e.g.\ for @unit3 → {unit1, unit2} → unit0@, reversing the DFS
-- pre-order may yield @[unit2, unit0, unit1]@, loading @unit2@ before @unit0@.
-- The full graph's topological sort handles shared ancestors correctly.
depLoadOrder :: Ord key => Graph (Graph.Node key payload) -> Graph.Node key payload -> [Graph.Node key payload]
depLoadOrder depGraph root =
  [ node
  | node <- reverse (topologicalSortG depGraph)
  , Set.member node.node_key reachableNames
  ]
  where
    reachableNames =
      Set.fromList
        [ node.node_key
        | node <- reachablesG depGraph [root]
        , node.node_key /= root.node_key
        ]

-- | Build 'CachedBuildPlans' for a unit's transitive dependency units.
--
-- This is equivalent to what Buck does before executing metadata:
--
-- > transitive_deps.project_as_json("dep_units")
-- > actions.write_json(dep_units_file, dep_units)
--
-- Uses the pre-computed unit dependency graph to query the transitive closure
-- via 'depLoadOrder' and collect cache paths for all dep units that have
-- @cached_unit.json@ files.
buildDepPlans :: Graph UnitDepNode -> Unit -> IO CachedBuildPlans
buildDepPlans depGraph unit =
  CachedBuildPlans . fmap plan <$> filterM (doesFileExist . (.node_payload)) (depLoadOrder depGraph selfNode)
  where
    plan node =
      CachedBuildPlan {
        name = JsonFs (toUnitId (stringToUnit node.node_key.string)),
        build_plan = node.node_payload
      }

    selfNode = Graph.DigraphNode {
      node_payload = unit.cache.cachedUnitPath,
      node_key = unit.name,
      node_dependencies = []
    }


-- | If the unit's @cached_unit.json@ exists from a prior build, return its path.
--
-- This is used before compilation to let 'withGhcMakeModule' restore the home unit via 'loadHomeUnit'.
loadHomeUnitCache :: UnitCache -> IO (Maybe OsPath)
loadHomeUnitCache unitCache =
  whenMaybeM (doesFileExist unitCache.cachedUnitPath) (pure unitCache.cachedUnitPath)

-- | Check whether a module's interface file (@.dyn_hi@) exists.
--
-- The interface file is the reliable indicator that a module was compiled in a prior build.
interfaceExists :: OsPath -> UnitName -> ModuleName -> IO Bool
interfaceExists outputDir name modName =
  doesFileExist (moduleHiPath outputDir name modName)

-- | Compute the set of all units with cache from a prior build.
cachedUnitsForProject :: Project -> IO (Set UnitName)
cachedUnitsForProject project =
  foldM check Set.empty (Map.toList project.units)
  where
    check acc (name, unit) =
      cacheExists unit.cache <&> \case
        True -> Set.insert name acc
        False -> acc

-- | Construct a 'BuildCache' from a 'Project' and output directory.
mkBuildCache :: OsPath -> Project -> BuildCache
mkBuildCache outputDir project =
  BuildCache {
    unitCached = \name ->
      maybe (pure False) (cacheExists . (.cache)) (Map.lookup name project.units),
    loadUnit = \name ->
      maybe (pure (Right Nothing)) (loadCachedUnit . (.cache)) (Map.lookup name project.units),
    interfaceExists = interfaceExists outputDir,
    cachedUnits = cachedUnitsForProject project
  }

-- | Load the 'CachedUnit' from @cached_unit.json@, if it exists.
--
-- Returns @Right Nothing@ if the file is absent, @Right (Just cu)@ on success,
-- @Left err@ on decode failure.
loadCachedUnit :: UnitCache -> IO (Either String (Maybe CachedUnit))
loadCachedUnit unitCache =
  OsPath.doesFileExist unitCache.cachedUnitPath >>= \case
    False -> pure (Right Nothing)
    True ->
      eitherDecodeFileStrict' pathFp >>= \case
        Left err ->
          pure (Left ("Failed to decode cached unit " ++ pathFp ++ ": " ++ err))
        Right cu -> pure (Right (Just cu))
  where
    pathFp = fromOsPath unitCache.cachedUnitPath
