module ScheduleTest where

import qualified Data.Set as Set
import Hedgehog (diff, property, withTests)
import Test.Data.Project (Component (..), GenUnit (..), ModuleKey (..), TaskKey (..))
import Test.Data.Scheduler (Task (..), Schedule (..))
import Test.Gen.ProjectBuild (sortSchedule)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

-- | These task lists have the same content but different orderings.
-- The dependency graph does not impose a total order, which we exploit
-- to produce different schedules.
tasks1, tasks2 :: [Task TaskKey Component]
(tasks1, tasks2) =
  (
    [meta2, mod20, meta1, mod10, meta0, mod00, mod01],  -- reverse unit order
    [meta1, mod10, meta0, mod00, mod01, meta2, mod20]  -- mixed order
  )
  where
    -- Unit 0: two modules, no deps
    meta0 = unitTask 0 Set.empty
    mod00 = moduleTask 0 0 [TaskMeta 0]
    mod01 = moduleTask 0 1 [TaskMeta 0]

    -- Unit 1: one module, depends on unit 0 and module 0_0
    meta1 = unitTask 1 (Set.singleton 0)
    mod10 = moduleTask 1 0 [TaskMeta 1, TaskCompile (moduleKey 0 0)]

    -- Unit 2: one module, depends on unit 1 and module 1_0
    meta2 = unitTask 2 (Set.singleton 1)
    mod20 = moduleTask 2 0 [TaskMeta 2, TaskCompile (moduleKey 1 0)]

    moduleTask unit number deps =
      Task {
        key = TaskCompile (moduleKey unit number),
        deps,
        value = ComponentModule (moduleKey unit number)
      }

    moduleKey unit number = ModuleKey {unit, number, errorVariant = Nothing}

    unitTask key depUnits =
      Task {
        key = TaskMeta key,
        deps = Set.map TaskMeta depUnits,
        value = ComponentUnit GenUnit {key, depUnits, modules = []}
      }

-- | Assert that 'sortSchedule' is sensitive to input order.
-- Two different permutations of the same tasks can produce different schedules.
-- This proves that shuffling before 'sortSchedule' in 'genSchedule'
-- is effective at producing diverse valid orderings.
test_sortScheduleOrder :: TestTree
test_sortScheduleOrder =
  testProperty "sortSchedule reorders tasks" $ withTests 1 $ property do
    diff (sort tasks1) (/=) (sort tasks2)
    diff (sort tasks1) (==) (sort tasks1)
  where
    sort tasks = [n.key | n <- (fst (sortSchedule tasks)).tasks]
