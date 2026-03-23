module Test.Data.Project where

import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Numeric.Natural (Natural)
import Test.Data.Scheduler (Task (..))
import Types.CachedDeps (CachedBuildPlans, CachedDeps)

-- | Error variant for modules that should fail compilation.
-- The variant determines both the generated source expression and the expected GHC diagnostic code.
data ErrorVariant =
  -- | @+ x@ where @x@ is undefined (diagnostic code 88464).
  UndefinedVariable
  |
  -- | @+ True@ causing a type mismatch with the @Int@ result (diagnostic code 83865).
  TypeMismatch
  deriving stock (Eq, Ord, Show)

errorDiagnosticCode :: ErrorVariant -> Natural
errorDiagnosticCode = \case
  UndefinedVariable -> 88464
  TypeMismatch -> 83865

-- | Used both as a Map key and to assemble file paths.
newtype UnitKey =
  UnitKey { number :: Int }
  deriving stock (Eq, Show)
  deriving newtype (Num, Real, Enum, Integral, Ord)

-- | The parameters determining content and paths of modules as well as some test behavior, like fixing errors.
-- Also used to index Maps.
data ModuleKey =
  ModuleKey {
    unit :: UnitKey,
    number :: Int,
    errorVariant :: Maybe ErrorVariant
  }
  deriving stock (Eq, Ord, Show)

-- | Data representing the project before the initial build is started, and that isn't related to scheduling or
-- resuming.
--
-- This exists mostly for the convenience of bundling all the data needed downstream, and to avoid repeated
-- reconstruction or duplicate lookups of the individual fields in classifiers, source generation, and others.
data InitialProject =
  InitialProject {
    -- | Map from each module to its dependency modules, aggregated across units.
    modules :: Map ModuleKey [ModuleKey],

    -- | Module and their deps that are expected to compile successfully.
    modulesSuccess :: Map ModuleKey [ModuleKey],

    -- | Module and their deps that are expected to fail.
    modulesError :: Map ModuleKey [ModuleKey],

    -- | Total number of units.
    -- For output.
    unitCount :: Int,

    -- | Total number of modules across all units.
    -- For output.
    moduleCount :: Int
  }
  deriving stock (Eq, Show)

-- | Metadata generated for a single module.
data GenModule =
  GenModule {
    key :: ModuleKey,
    -- | Deps for the initial build.
    deps :: Set ModuleKey,
    -- | Additional deps for the resume build.
    resumeDeps :: Maybe (Set ModuleKey),
    -- | Whether this module uses Template Haskell splices.
    th :: Bool,
    -- | Number of top-level value bindings to generate in the source file.
    bindings :: Int,
    -- | Indexes of external dependency packages imported by this module.
    extDeps :: Set Int
  }
  deriving stock (Eq, Show)

-- | Module as used by the build pipeline. Contains only the data needed for scheduling, source generation, and cache
-- writing.
data BuildModule =
  BuildModule {
    key :: ModuleKey,
    deps :: Set ModuleKey,
    -- | Whether this module uses Template Haskell splices.
    th :: Bool,
    -- | Number of top-level value bindings to generate in the source file.
    bindings :: Int,
    -- | Indexes of external dependency packages imported by this module.
    extDeps :: Set Int
  }
  deriving stock (Eq, Show)

-- | Metadata generated for a single unit.
-- During generation this is @GenUnit GenModule@ (carrying 'resumeDeps');
-- after generation it becomes @GenUnit BuildModule@, projected for the initial and resume build with different deps.
data GenUnit a =
  GenUnit {
    key :: UnitKey,
    depUnits :: Set UnitKey,
    modules :: [a]
  }
  deriving stock (Eq, Show)

-- | Used to track tasks in the scheduler.
data TaskKey =
  TaskMeta UnitKey
  |
  TaskCompile ModuleKey
  deriving stock (Eq, Ord, Show)

taskModuleKeys :: Foldable t => t TaskKey -> [ModuleKey]
taskModuleKeys keys =
  [key | TaskCompile key <- toList keys]

-- | A unit's cache data, as provided by Buck.
data UnitCache =
  UnitCache {
    -- | The index of build plans of dependency units.
    -- This is read and decoded before being passed it to the compile handler, so we don't have to roundtrip through
    -- JSON files in the test.
    cachedBuildPlans :: Maybe CachedBuildPlans
  }
  deriving stock (Eq, Show)

-- | A module's cache data, as provided by Buck.
data ModuleCache =
  ModuleCache {
    -- | The home unit's build plan and arguments.
    cachedUnit :: FilePath,

    -- | The interfaces of the dependency closure across the project.
    -- Decoded outside of worker handlers, so no JSON file.
    cachedDeps :: CachedDeps
  }
  deriving stock (Eq, Show)

-- | Payload of a build step, used by 'Test.Build.runSchedule' to dispatch to
-- 'Internal.Metadata.computeMetadata' or 'Internal.Compile.Make.compileModuleWithDepsInHpt'.
data Component =
  ComponentUnit (GenUnit BuildModule)
  |
  ComponentModule ModuleKey
  deriving stock (Eq, Show)

-- | 'Component' bundled with cache paths for resume builds.
data ResumeComponent =
  ResumeUnit (GenUnit BuildModule) UnitCache
  |
  ResumeModule ModuleKey ModuleCache
  deriving stock (Eq, Show)

type BuildTask = Task TaskKey Component

type ResumeBuildTask = Task TaskKey ResumeComponent
