-- | Description: Logic interfacing with the worker to start metadata and compile tasks.
module Test.Build where

import Data.Foldable (fold, for_)
import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC (mkModule, mkModuleName)
import GHC.Driver.Session (DynFlags (..), GhcMode (..))
import GHC.Types.Error (diagnosticCodeNumber)
import GHC.Unit (stringToUnit)
import Internal.Compile.Make (compileModuleWithDepsInHpt)
import Internal.DynFlags (modifyGlobalFlags)
import Internal.Metadata (computeMetadata)
import Internal.Session (withGhcMakeModule)
import Numeric.Natural (Natural)
import Prelude hiding (log)
import qualified System.Directory.OsPath as OsDir
import System.Directory.OsPath (createDirectoryIfMissing)
import System.OsPath.Extra (OsPath, fromOsPath, osp, takeDirectory, (</>))
import Test.Data.BuildSystem (BuildResult (..))
import Test.Data.Env (MaxJobs, SessionEnv (..), TestEnv (..))
import Test.Data.Project (
  BuildModule (..),
  Component (..),
  GenUnit (..),
  ModuleCache (..),
  ModuleKey (..),
  ResumeComponent (..),
  TaskKey (..),
  UnitCache (..),
  errorDiagnosticCode,
  taskModuleKeys,
  )
import Test.Data.Scheduler (RequestFailure (..), RequestResult (..), Schedule (..), SchedulerState (..))
import Test.Data.TestLog (DiagnosticEntry (..), TestLog (..))
import Test.Log (withTestLog)
import Test.Path (compileTmpDir, extDepName, moduleName, moduleSourcePath, unitDir, unitName, unitOutputDir, unitTmpDir)
import Test.Scheduler (initScheduler, runScheduler)
import qualified Types.Args as Args
import Types.Args (Args (..))
import Types.BuckArgs (IsInterpreted (Compiled))
import Types.BuildPlan.Incremental (BuckHashesPath (..), BuildPlanPath (..))
import Types.Env (Env (..))
import Types.Target (ModuleTarget (..), TargetSpec (..))

-- | Decide whether a worker task was successful based on the emitted diagnostic codes.
-- A failing build is 'ExpectedFailure' only if all emitted codes are in @expectedCodes@.
--
-- We don't require all expected codes to be emitted, since that may be difficult to decide in the face of concurrent
-- builds.
requestResult :: Set Natural -> Bool -> TestLog -> RequestResult
requestResult expectedCodes success TestLog {diagnostics}
  | success = RequestSuccess
  | null unexpected = RequestFailure ExpectedFailure
  | otherwise = RequestFailure (UnexpectedDiagnostics unexpected)
  where
    unexpected = Set.difference actualCodes expectedCodes
    actualCodes = Set.fromList (mapMaybe (fmap diagnosticCodeNumber . (.code)) diagnostics)

-- | Execute a worker task with a fresh logger and extract diagnostic data from the log afterwards.
runBuildTask ::
  SessionEnv ->
  String ->
  OsPath ->
  Set Natural ->
  (Env -> IO Bool) ->
  IO RequestResult
runBuildTask env label tempName expectedCodes action =
  withTestLog False label \ (log, logVar) -> do
    let taskEnv = env.env {log, args = env.env.args {Args.tempDir = Just tempDir}}
    OsDir.createDirectoryIfMissing True tempDir
    success <- action taskEnv
    testLog <- readIORef logVar
    pure (requestResult expectedCodes success testLog)
  where
    tempDir = env.tempDir </> tempName

-- | Execute a metadata task.
-- We currently don't support expected errors during metadata steps, so no diagnostic codes are passed to
-- 'runBuildTask'.
runMetadata :: SessionEnv -> (GenUnit BuildModule -> Args) -> GenUnit BuildModule -> IO RequestResult
runMetadata env mkArgs unit = do
  let args = mkArgs unit
      srcFiles = [fromOsPath (env.sourceDir </> moduleSourcePath gm.key) | gm <- unit.modules]
  -- Ensure the build plan output directory exists for incremental state files
  for_ args.buildPlan \ (BuildPlanPath bp) ->
    createDirectoryIfMissing True (takeDirectory bp)
  runBuildTask env "metadata" (unitTmpDir unit.key) [] \ taskEnv -> do
    fst <$> computeMetadata taskEnv {args = args {ghcOptions = args.ghcOptions ++ srcFiles}}

compileTarget :: ModuleKey -> ModuleTarget
compileTarget key =
  ModuleTarget {
    mod = mkModule (stringToUnit (unitName key.unit)) (mkModuleName (moduleName key))
  }

-- | Execute a compile task.
runCompile :: SessionEnv -> (ModuleKey -> (Args, Set Natural)) ->  ModuleKey -> IO RequestResult
runCompile env mkArgs key = do
  runBuildTask env "compile" (compileTmpDir key) codes \ taskEnv -> do
    let compileEnv = taskEnv {args}
        target = compileTarget key
    result <- withGhcMakeModule Compiled target compileEnv \ _targetSpec -> do
      modifyGlobalFlags \ d -> d {ghcMode = CompManager}
      compileModuleWithDepsInHpt compileEnv.log (TargetModule target)
    pure (isJust result)
  where
    (args, codes) = mkArgs key

staticMetaArgs :: [String]
staticMetaArgs = [
  "-i",
  "-hide-all-packages",
  "-include-pkg-deps",
  "-no-link",
  "-dynamic",
  "-fbyte-code-and-object-code",
  "-fprefer-byte-code",
  "-fPIC",
  "-osuf", "dyn_o",
  "-hisuf", "dyn_hi",
  "-package", "base"
  ]

-- | Assemble the arguments passed to unit state initialization in a metadata step, resembling how the Buck rules
-- provide them.
--
-- When @useIncremental@ is 'True', sets 'Args.buildPlan' and 'Args.sourceHashes' to enable incremental metadata.
-- The buck_source_hashes path is derived from the unit key: @tempDir/unitN/source_hashes.json@.
-- Each unit reads its own per-unit file, matching the ghc-server setup where each metadata request has its own
-- buck_source_hashes path containing only that unit's sources.
metadataArgs :: SessionEnv -> Bool -> GenUnit BuildModule -> Args
metadataArgs env useIncremental GenUnit {key, modules, depUnits} =
  env.shared.baseArgs {
    buildPlan = Just buildPlanPath,
    sourceHashes = if useIncremental then Just perUnitMetaPath else Nothing,
    ghcOptions = staticMetaArgs ++ extDepDbArgs ++ thArgs ++ extDepPkgArgs ++ metaArgs ++ unitDepArgs
  }
  where
    metaArgs = [
      "-this-unit-id", unitName key,
      "-dep-json=" ++ fromOsPath (sessionTmpDir </> [osp|dep.json|]),
      "-dep-makefile=" ++ fromOsPath (sessionTmpDir </> [osp|dep.make|]),
      "-odir", fromOsPath outDir,
      "-hidir", fromOsPath outDir
      ]

    thArgs
      | any (.th) modules = ["-package", "template-haskell"]
      | otherwise = []

    allExtDeps = fold [m.extDeps | m <- modules]

    extDepDbArgs = concatMap (\db -> ["-package-db", db]) env.extDepDbs

    extDepPkgArgs = concatMap (\ i -> ["-package", extDepName i]) (Set.toList allExtDeps)

    unitDepArgs = concatMap (\ d -> ["-package-id", unitName d]) depUnits

    sessionTmpDir = env.tempDir </> unitTmpDir key

    outDir = env.tempDir </> unitOutputDir key

    perUnitMetaPath = BuckHashesPath (env.tempDir </> unitDir key </> [osp|source_hashes.json|])

    buildPlanPath = BuildPlanPath (outDir </> [osp|build-plan.json|])

-- | Add Buck cache paths for dependency build plans to 'metadataArgs' for a resume build metadata step.
resumeMetadataArgs :: SessionEnv -> Bool -> UnitCache -> GenUnit BuildModule -> Args
resumeMetadataArgs env useIncremental cache unit =
  (metadataArgs env useIncremental unit) {Args.cachedBuildPlans = cache.cachedBuildPlans}

errorCodeSet :: ModuleKey -> Set Natural
errorCodeSet key =
  foldMap (Set.singleton . errorDiagnosticCode) key.errorVariant

-- | The initial build always expects to encounter the diagnostics with which a module was generated.
initialCompileArgs :: SessionEnv -> ModuleKey -> (Args, Set Natural)
initialCompileArgs env key =
  (env.shared.baseArgs, errorCodeSet key)

-- | Add Buck cache paths for dependency build plans to the 'Args' passed to a resume build compile step.
-- The resume build only expects diagnostics when @fixErrors@ is 'False'.
resumeCompileArgs :: SessionEnv -> Bool -> ModuleCache -> ModuleKey -> (Args, Set Natural)
resumeCompileArgs env fixErrors ModuleCache {cachedUnit, cachedDeps} key = do
  (args, codes)
  where
    args = env.shared.baseArgs {homeUnit = Just cachedUnit, Args.cachedDeps = Just cachedDeps}

    codes
      | fixErrors = mempty
      | otherwise = errorCodeSet key

-- | Handlers for build steps specialized to the initial build's requirements.
initialStrategy :: SessionEnv -> Bool -> Component -> IO RequestResult
initialStrategy env useIncremental = \case
  ComponentUnit unit -> runMetadata env (metadataArgs env useIncremental) unit
  ComponentModule key -> runCompile env (initialCompileArgs env) key

-- | Handlers for build steps specialized to the resume build's requirements.
resumeStrategy :: SessionEnv -> Bool -> Bool -> ResumeComponent -> IO RequestResult
resumeStrategy env useIncremental fixErrors = \case
  ResumeUnit unit cache -> runMetadata env (resumeMetadataArgs env useIncremental cache) unit
  ResumeModule key cache -> runCompile env (resumeCompileArgs env fixErrors cache) key

-- | Extract the data required for properties and classifiers from the final state of the scheduler after a build.
buildResult :: SchedulerState TaskKey component -> BuildResult
buildResult state =
  BuildResult {
    failures = state.failures,
    completed = state.completed,
    succeeded,
    failedModules = taskModuleKeys failedModules,
    hasErrors = not (null failedModules)
  }
  where
    succeeded = Set.difference state.completed failedModules

    failedModules = Map.keysSet state.failures

-- | Run a schedule of build tasks to completion.
runSchedule ::
  -- | Maximum number of jobs that should be executed concurrently.
  MaxJobs ->
  -- | Task dispatch function that calls the worker.
  (component -> IO RequestResult) ->
  -- | Initial set of tasks keys that are treated as completed, representing the unmodified modules in a resume build.
  -- Only used to decide whether dependencies are available.
  Set TaskKey ->
  -- | All tasks that will be executed.
  -- This does _not_ discard the completed tasks in the previous argument.
  Schedule TaskKey component ->
  IO BuildResult
runSchedule maxJobs dispatch completed tasks =
  buildResult <$> runScheduler schedulerEnv initialState
  where
    (schedulerEnv, initialState) = initScheduler maxJobs dispatch tasks completed
