module Test.Gen.Project where

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import Data.Set (Set, (\\))
import Data.Traversable (mapAccumM)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Gen (small)
import qualified Hedgehog.Range as Range
import Test.Data.Env (MaxModulesPerUnit, TestConfig (..))
import Test.Data.Project (
  ErrorVariant (..),
  GenModule (..),
  GenUnit (..),
  InitialProject (..),
  ModuleKey (..),
  UnitKey (..),
  )

-- | Choose whether a module should contain an error with 20% likelihood, with error variants distributed uniformly.
-- With a static likelihood per module, a successful project build gets less likely with increasing module count, so we
-- might want to improve this.
-- Though shrinking happens towards the first element (no error), so we're at least guaranteed to be biased towards
-- that.
genModuleError :: Gen (Maybe ErrorVariant)
genModuleError =
  Gen.frequency [
    (8, pure Nothing),
    (1, pure (Just UndefinedVariable)),
    (1, pure (Just TypeMismatch))
  ]

genModuleKey :: UnitKey -> Int -> Gen ModuleKey
genModuleKey unit number = do
  errorVariant <- genModuleError
  pure ModuleKey {..}

-- | Choose dependencies for a module.
--
-- There are two sets: the initial dependencies, and those that are added for the resume build.
--
-- The @pool@ contains all modules that are eligible as dependencies.
-- After generating the module, it is added to the pool.
-- This ensures that the dep graph is cycle-free.
--
-- We choose a smaller set for the resume deps to reduce the average build duration.
-- We remove deps from the resume deps that are already in the base set.
--
-- At the moment, we're not removing any deps, since that just relaxes the build, which is less likely to cause issues,
-- but it would be nice to have.
-- I could imagine that if the schedule of the resume build would violate the dependency order of the first build, some
-- stale data in GHC may contain references that won't find their data.
genModule :: Set ModuleKey -> ModuleKey -> Gen (Set ModuleKey, GenModule)
genModule pool key = do
  deps <- Gen.subset pool
  extraDeps <- small $ Gen.subset pool
  let extra = extraDeps \\ deps
      resumeDeps = if null extra then Nothing else Just extra
  pure (Set.insert key pool, GenModule {key, deps, resumeDeps, th = False})

-- | Generate all home modules for a unit.
genUnitModules ::
  UnitKey ->
  -- | The @deps@ are the units from which we're allowed to sample module dependencies in 'genModule'.
  [GenUnit GenModule] ->
  -- | Number of modules this unit should consist of.
  Int ->
  Gen (GenUnit GenModule)
genUnitModules unitKey deps numMods = do
  moduleKeys <- traverse (genModuleKey unitKey) [0 .. numMods - 1]
  modules <- snd <$> mapAccumM genModule modulePool moduleKeys
  pure GenUnit {key = unitKey, depUnits, modules}
  where
    depUnits = Set.fromList [unit.key | unit <- deps]

    modulePool = Set.fromList [m.key | u <- deps, m <- u.modules]

-- | Generate a unit with dependencies drawn from @pool@.
--
-- We choose unit dependencies primarily for choosing module dependencies from; but the field in 'GenUnit' is also used
-- to synthesize @-package-id@ flags.
genUnit ::
  MaxModulesPerUnit ->
  IntMap (GenUnit GenModule) ->
  UnitKey ->
  Gen (IntMap (GenUnit GenModule), GenUnit GenModule)
genUnit maxModulesPerUnit unitPool key = do
  deps <- Gen.subsequence (IntMap.elems unitPool)
  numMods <- Gen.int (Range.linear 1 (fromIntegral maxModulesPerUnit))
  unit <- genUnitModules key deps numMods
  pure (IntMap.insert key.number unit unitPool, unit)

-- | Transform the project's units into the data shapes commonly required downstream.
initialProject :: [GenUnit GenModule] -> InitialProject
initialProject genUnits =
  InitialProject {
    modules,
    modulesSuccess,
    modulesError,
    unitCount = length genUnits,
    moduleCount = Map.size modules
  }
  where
    (modulesSuccess, modulesError) =
      Map.partitionWithKey (\ ModuleKey {errorVariant} _ -> isNothing errorVariant) modules

    modules = Map.fromList [(gm.key, Set.toList gm.deps) | u <- genUnits, gm <- u.modules]

-- | Generate all data for the project.
--
-- This mostly corresponds to the initial build only, though the module data includes the additional deps used for the
-- resume build, for complexity reduction reasons.
genProject :: TestConfig -> Gen (InitialProject, [GenUnit GenModule])
genProject TestConfig {maxUnits, maxModulesPerUnit} = do
  numUnits <- Gen.integral (Range.linear 1 (fromIntegral maxUnits))
  (_, units) <- mapAccumM (genUnit maxModulesPerUnit) IntMap.empty [0 .. numUnits - 1]
  pure (initialProject units, units)
