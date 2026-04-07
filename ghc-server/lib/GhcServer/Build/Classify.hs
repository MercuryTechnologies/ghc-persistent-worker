-- | Build system logic: request expansion, task classification, and result collection.
--
-- This module implements the external build coordinator's perspective — deciding /what/ to
-- build and how to map user requests into scheduler tasks.  It corresponds to the role
-- an external build system (like Buck) plays: expanding targets into effective work items,
-- classifying them into metadata and compile tasks, and interpreting completed scheduler
-- state as a build result.
--
-- This module does not interact with GHC, the worker, or the cache.
module GhcServer.Build.Classify where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC (ModuleName)
import GhcServer.Build.Schedule (
  BuildStatus,
  TaskKey (..),
  compileTasksFromSources,
  metadataTasks,
  )
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.Request (
  EffectiveUnit (..),
  ScheduleRequest (..),
  UnitRequest (..),
  effectiveIsCompile,
  effectiveUnitName,
  )
import GhcServer.Data.Unit (Project (..), Unit (..), UnitName (..))
import GhcServer.Scheduler (Phase (..), Task (..))

-- | Determine the effective request for each unit, expanding an empty 'ScheduleRequest'
-- to 'UnitAll' for every unit and adding transitive deps.
--
-- For explicit requests, the order from the CLI is preserved. Implicit deps are
-- prepended in the order they are discovered by walking explicit targets' dependency trees.
effectiveRequests :: Project -> ScheduleRequest -> [EffectiveUnit]
effectiveRequests project request
  | null request.steps = [Explicit name UnitAll | name <- Map.keys project.units]
  | otherwise = implicitEntries ++ map (uncurry Explicit) request.steps
  where
    requestMap = Map.fromList request.steps

    implicitEntries =
      [
        ImplicitDep name
        | name <- Set.toAscList (transitiveDeps project (map fst request.steps))
        , not (Map.member name requestMap)
      ]

-- | Classify a 'ScheduleRequest' into active metadata tasks and pending compile tasks.
--
-- All units receive a metadata task and pending compile tasks, regardless of cache state.
-- Whether to actually run metadata is decided at dispatch time based on the @cached@ flag
-- embedded in 'Metadata' (computed here from the @cachedUnits@ set).
-- Whether to promote compile tasks is decided by the 'enabled' flag set here, which
-- reflects whether the unit was explicitly requested for compilation.
--
-- The @rebuild@ and @cached@ flags are embedded in the task values
-- for dispatch-time skip decisions, eliminating the need for mutable per-request state.
classifyBuildRequest ::
  Set UnitName ->
  BuildEnv ->
  ScheduleRequest ->
  IO ([Task TaskKey 'Resolved BuildStatus], [Task TaskKey 'Pending BuildStatus])
classifyBuildRequest cachedUnits env request =
  pure (metaTasks, pendingTasks)
  where
    reqs = effectiveRequests env.project request

    rebuild = request.recompile

    metaTasks = metadataTasks env.project cachedUnits request.rebuild (map effectiveUnitName reqs)

    pendingTasks = concatMap unitCompileTasks reqs

    unitCompileTasks eu =
      case Map.lookup (effectiveUnitName eu) env.project.units of
        Just unit ->
          compileTasksFromSources
            (effectiveUnitName eu)
            rebuild
            (effectiveIsCompile rebuild eu)
            unit.sources
        Nothing -> []

-- | Collect a 'BuildResult' from the scheduler's completed and failure sets.
collectBuildResult :: Set (TaskKey 'Resolved) -> Map (TaskKey 'Resolved) String -> BuildResult
collectBuildResult _completed failures =
  BuildResult {
    success = null metaErrs && null compErrs,
    metadataErrors = metaErrs,
    compileErrors = compErrs
  }
  where
    metaErrs = [(name, msg) | (MetaTask name, msg) <- Map.toList failures]

    compErrs = [(name, modName, msg) | (ResolvedModule name modName, msg) <- Map.toList failures]

-- | Compute the transitive closure of unit dependencies from a set of root unit names.
transitiveDeps :: Project -> [UnitName] -> Set UnitName
transitiveDeps project =
  foldl' addDeps Set.empty
  where
    addDeps acc name
      | Set.member name acc = acc
      | otherwise = case Map.lookup name project.units of
        Nothing -> acc
        Just unit -> foldl' addDeps (Set.insert name acc) unit.depUnits

-- | A build result.
data BuildResult =
  BuildResult {
    success :: Bool,
    metadataErrors :: [(UnitName, String)],
    compileErrors :: [(UnitName, ModuleName, String)]
  }
  deriving stock (Show)
