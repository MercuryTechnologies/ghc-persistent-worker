module Test.Gen.ProjectBuild where

import Data.Foldable (fold)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified GHC.Data.Graph.Directed as G (Node (..))
import GHC.Data.Graph.Directed (graphFromEdgedVerticesOrd, topologicalSortG)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import Test.Data.Env (TestConfig)
import Test.Data.Project (
  BuildModule (..),
  BuildTask,
  Component (..),
  GenModule (..),
  GenUnit (..),
  TaskKey (..),
  UnitKey,
  )
import Test.Data.ProjectBuild (ProjectBuild (..), scheduleKeys)
import Test.Data.Scheduler (Schedule (..), Task (..))
import Test.Gen.Project (genProject)
import Test.Gen.Resume (DepGraph, genResumePlan)

-- | A metadata task depends on other metadata tasks.
metaTask :: GenUnit BuildModule -> Set UnitKey -> BuildTask
metaTask unit depUnits =
  Task {
    key = TaskMeta unit.key,
    deps = Set.map TaskMeta depUnits,
    value = ComponentUnit unit
  }

-- | For a compile task, depend on the home unit's metadata as well as the module dependencies' compile tasks.
moduleTask :: GenUnit a -> BuildModule -> BuildTask
moduleTask GenUnit {key = unitKey} BuildModule {key = moduleKey, deps} =
  Task {
    key = TaskCompile moduleKey,
    deps = Set.insert (TaskMeta unitKey) (Set.map TaskCompile deps),
    value = ComponentModule moduleKey
  }

unitTasks :: GenUnit BuildModule -> [BuildTask]
unitTasks unit =
  metaTask unit unit.depUnits : (moduleTask unit <$> unit.modules)

-- | Create a dependency graph from a set of tasks and sort it such that they can be executed by GHC in correct
-- dependency order.
--
-- The tasks contain both units (metadata tasks) and modules (compile tasks), where metadata tasks depend only on other
-- units, while compile tasks depend on other modules as well as their home unit's metadata.
sortSchedule :: [Task TaskKey Component] -> (Schedule TaskKey Component, DepGraph)
sortSchedule tasks =
  (Schedule {tasks = G.node_payload <$> sorted}, graph)
  where
    graphTasks = [G.DigraphNode n n.key (Set.toList n.deps) | n <- tasks]
    graph = graphFromEdgedVerticesOrd graphTasks
    sorted = reverse (topologicalSortG graph)

-- | Generate a random task schedule for the given units constrained by their module dependencies.
--
-- The list is shuffled and then sorted.
-- Because graph sorting imposes a partial order using only the dependencies, shuffling ensures that we don't get the
-- same sequence of compile tasks every time for the same dep graph, which would cluster modules from units in
-- dependency order.
--
-- This simulates Buck's ability to compile multiple units in parallel if their module dep graph allows it.
genSchedule :: [GenUnit BuildModule] -> Gen (Schedule TaskKey Component, DepGraph)
genSchedule =
  fmap sortSchedule .
  Gen.shuffle .
  concatMap unitTasks

-- | Discard the 'resumeDeps' for each module for the initial build.
initialBuildUnit :: GenUnit GenModule -> GenUnit BuildModule
initialBuildUnit unit =
  unit {
    modules = [BuildModule {key, deps, th, bindings, extDeps}
              | GenModule {key, deps, th, bindings, extDeps} <- unit.modules]
  }

-- | Merge the 'resumeDeps' into the effective deps for the resume build.
resumeBuildUnit :: GenUnit GenModule -> GenUnit BuildModule
resumeBuildUnit unit =
  unit {
    modules = [BuildModule {key, deps = deps <> fold resumeDeps, th, bindings, extDeps}
              | GenModule {key, deps, resumeDeps, th, bindings, extDeps} <- unit.modules]
  }

-- | Generate the full dataset used by a single test run.
genProjectBuild :: TestConfig -> Gen ProjectBuild
genProjectBuild conf = do
  (initial, units) <- genProject conf
  (schedule, _) <- genSchedule (initialBuildUnit <$> units)
  (resumeSchedule, resumeGraph) <- genSchedule (resumeBuildUnit <$> units)
  resumePlan <- genResumePlan initial units resumeGraph
  pure ProjectBuild {
    initial,
    schedule,
    resumeSchedule,
    resumePlan,
    allKeys = scheduleKeys schedule
  }
