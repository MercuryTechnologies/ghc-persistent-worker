module Test.Data.ProjectBuild where

import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Test.Data.Project (Component (..), InitialProject (..), ModuleKey (..), TaskKey (..))
import Test.Data.Scheduler (Schedule (..), Task (..))

-- | Metadata that indicates which modules should be cleaned up and rebuilt.
data RebuildSet =
  RebuildSet {
    -- | Module keys that need rebuilding (transitive dependent closure of modified + added-dep modules).
    moduleKeys :: Set ModuleKey,

    -- | 'moduleKeys' wrapped as @TaskCompile@ plus the metadata tasks for affected units.
    -- Used to determine the set initially marked as completed in the scheduler.
    allAffectedKeys :: Set TaskKey,

    -- | Convenience flag.
    hasChanges :: Bool
  }
  deriving stock (Eq, Show)

-- | Configuration for the resume build.
data ResumePlan =
  ResumePlan {
    -- | When 'True' and the initial build had error modules, rewrite their sources to remove the error before
    -- rebuilding.
    -- Ignored when the initial build succeeded.
    fixErrors :: Bool,

    -- | Succeeding modules whose sources are changed between builds, with their initial deps.
    moduleMutations :: Map ModuleKey [ModuleKey],

    -- | Additional dependencies to add for the resume build.
    -- Each entry maps a module to @(additional, total)@: additional deps and the total set of deps when combined with
    -- the initial deps.
    depMutations :: Map ModuleKey (Set ModuleKey, [ModuleKey]),

    rebuild :: RebuildSet
  }
  deriving stock (Eq, Show)

-- | The full set of data used by one test run.
data ProjectBuild =
  ProjectBuild {
    -- | Data required for the initial build.
    initial :: InitialProject,

    -- | The task schedule for the initial build.
    schedule :: Schedule TaskKey Component,

    -- | Data required for the resume build.
    resumePlan :: ResumePlan,

    -- | The task schedule for the resume build.
    resumeSchedule :: Schedule TaskKey Component,

    -- | All task keys in the schedule, for asserting completed builds.
    allKeys :: Set TaskKey,

    -- | When 'True', use incremental metadata (only re-downsweep changed modules).
    incrementalBuildPlan :: Bool
  }
  deriving stock (Show)

scheduleKeys :: Schedule TaskKey Component -> Set TaskKey
scheduleKeys sched =
  Set.fromList [n.key | n <- sched.tasks]
