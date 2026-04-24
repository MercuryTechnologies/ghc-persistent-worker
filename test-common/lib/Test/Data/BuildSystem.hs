module Test.Data.BuildSystem where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Test.Data.Env (SessionEnv)
import Test.Data.Project (Component, ModuleKey, TaskKey, ResumeComponent)
import Test.Data.ProjectBuild (RebuildSet)
import Test.Data.Scheduler (RequestFailure, Schedule (..))

-- | Output of the scheduler for a single build.
data BuildResult =
  BuildResult {
    failures :: Map TaskKey RequestFailure,
    completed :: Set TaskKey,
    succeeded :: Set TaskKey,
    failedModules :: [ModuleKey],
    hasErrors :: Bool
  }

-- | Abstraction of the operations that would be performed by Buck, just for convenience.
data BuildSystem =
  BuildSystem {
    -- | Write cache files and bundle cache paths into the schedule tasks.
    writeCache ::
      Schedule TaskKey Component ->
      IO (Schedule TaskKey ResumeComponent)
    ,

    -- | Execute the initial build with clean cache.
    runInitialBuild :: Schedule TaskKey Component -> IO BuildResult
    ,

    -- | Execute the resume build with cache files provided in the 'ResumeComponent's.
    runResumeBuild ::
      SessionEnv ->
      Bool ->
      Set TaskKey ->
      Schedule TaskKey ResumeComponent ->
      IO BuildResult
    ,

    -- | Delete the artifacts produced by all modules that are scheduled to be rebuilt during the resume build.
    cleanArtifacts :: BuildResult -> RebuildSet -> IO ()
  }
