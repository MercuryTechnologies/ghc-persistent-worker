module Test.Resume where

import Data.Foldable (for_)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified System.File.OsPath as OsPath
import System.OsPath (OsPath, osp, (<.>), (</>))
import Test.Data.BuildSystem (BuildResult (..), BuildSystem (..))
import Test.Data.Env (SessionEnv (..))
import Test.Data.Project (InitialProject (..), ModuleKey (..), ResumeComponent, TaskKey)
import Test.Data.ProjectBuild (ProjectBuild (..), RebuildSet (..), ResumePlan (..))
import Test.Data.Scheduler (Schedule (..), Task (..))
import Test.Data.SourceMode (SourceMode (..), SourceRewrite (..))
import Test.Path (moduleOutputBase, moduleSourcePath, removeIfExists)
import Test.Source (moduleSource)

-- | Combine all mutations into one set that eliminates duplication, since added deps can overlap with the other
-- mutations.
sourceRewrites :: InitialProject -> ResumePlan -> Bool -> Map ModuleKey SourceRewrite
sourceRewrites InitialProject {modulesError} plan fixErrors =
  fixedSources <> modifiedSources <> addedDepSources
  where
    fixedSources
      | fixErrors = fmap (\ deps -> SourceRewrite {mode = SourceFixed, deps, th = False}) modulesError
      | otherwise = []

    modifiedSources = fmap (\ deps -> SourceRewrite {mode = SourceModified, deps, th = False}) plan.moduleMutations

    addedDepSources =
      plan.depMutations <&> \ (_, total) -> SourceRewrite {mode = SourceNormal, deps = total, th = False}

-- | Update all files for which a 'SourceRewrite' was constructed.
rewriteResumeSources ::
  OsPath ->
  Map ModuleKey SourceRewrite ->
  IO ()
rewriteResumeSources sourceDir rewrites =
  for_ (Map.toList rewrites) \ (key, rewrite) ->
    OsPath.writeFile (sourceDir </> moduleSourcePath key) (moduleSource rewrite.th rewrite.mode key rewrite.deps)

-- | Remove all tasks from the resume schedule that don't require rebuild.
--
-- TODO check whether this is still necessary
trimResumeSchedule ::
  BuildResult ->
  RebuildSet ->
  [Task TaskKey ResumeComponent] ->
  (Schedule TaskKey ResumeComponent, Set TaskKey)
trimResumeSchedule initialResult rebuild tasks
  | rebuild.hasChanges || initialResult.hasErrors = (Schedule (filter include tasks), unmodified)
  | otherwise = (Schedule tasks, Set.empty)
  where
    unmodified = Set.difference initialResult.succeeded rebuild.allAffectedKeys

    include task =
      not (Set.member task.key initialResult.succeeded) || Set.member task.key rebuild.allAffectedKeys

-- | Delete build artifacts, modify the sources and write cache files for the resume build.
setupResumeBuild ::
  BuildSystem ->
  SessionEnv ->
  ProjectBuild ->
  BuildResult ->
  IO (Schedule TaskKey ResumeComponent)
setupResumeBuild buildSys prevEnv ProjectBuild {initial, resumePlan, resumeSchedule} initialResult = do
  buildSys.cleanArtifacts initialResult resumePlan.rebuild
  rewriteResumeSources prevEnv.sourceDir rewrites
  buildSys.writeCache resumeSchedule
  where
    rewrites = sourceRewrites initial resumePlan fixErrors

    fixErrors = resumePlan.fixErrors && initialResult.hasErrors

-- | Remove unmodified modules and units from the resume schedule and run the resume build.
executeResumeBuild ::
  BuildSystem ->
  SessionEnv ->
  ProjectBuild ->
  BuildResult ->
  Schedule TaskKey ResumeComponent ->
  IO BuildResult
executeResumeBuild buildSys env build initialResult schedule = do
  let (resumeTasks, unmodified) = trimResumeSchedule initialResult build.resumePlan.rebuild schedule.tasks
  buildSys.runResumeBuild env fixErrors unmodified resumeTasks
  where
    fixErrors = build.resumePlan.fixErrors && initialResult.hasErrors

-- | Remove all objects and interfaces corresponding to modules that need rebuilding.
-- TODO verify that this is effective
-- TODO do we really need failedModules here or should that be in moduleKeys?
-- If an error module wasn't built (and therefore didn't fail) because the build terminated before it was reached, will
-- it be included here?
cleanResumeArtifacts :: OsPath -> BuildResult -> RebuildSet -> IO ()
cleanResumeArtifacts tempDir initialResult rebuild =
  for_ (initialResult.failedModules ++ Set.toList rebuild.moduleKeys) \ key -> do
    let base = tempDir </> moduleOutputBase key
    removeIfExists (base <.> [osp|dyn_o|])
    removeIfExists (base <.> [osp|dyn_hi|])
