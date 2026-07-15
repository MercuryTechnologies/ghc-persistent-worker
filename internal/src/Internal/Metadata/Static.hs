{-# LANGUAGE CPP #-}

module Internal.Metadata.Static where

import Data.Set (Set)
import GHC.Driver.Env (HscEnv (..))
import GHC.Unit (UnitId (..))
import Types.CachedDeps (CachedBuildPlans (..))

#if defined(UNIT_INDEX)

import Control.Exception (throwIO)
import Data.Aeson (eitherDecodeFileStrict')
import Data.Coerce (coerce)
import qualified Data.Set as Set
import Data.Traversable (for)
import qualified Data.Version as Version
import GHC (ModuleName (..))
import qualified GHC.Data.ShortText as ST
import GHC.Unit (GenericUnitInfo (..), PackageId (..), PackageName (..), UnitDatabase (..), UnitInfo)
import GHC.Unit.Env (UnitEnv (..))
import GHC.Unit.Finder (initFinderCache)
import GHC.Unit.State (UnitIndex (..), newUnitIndex)
import System.OsPath.Extra (fromOsPath, toOsPath)
import Types.CachedDeps (CachedBuildPlan (..), JsonFs (..))

#else

import GHC (GhcException (..))
import GHC.Utils.Panic (throwGhcExceptionIO)

#endif

-- | Read the module list of each unit in the transitive closure and make the units visible as external packages,
-- returning the set of static dependency units.
prepareStaticSession :: CachedBuildPlans -> HscEnv -> IO (HscEnv, Set UnitId)

#if defined(UNIT_INDEX)

prepareStaticSession plans hsc_env = do
  units <- readStaticUnits plans
  hsc_FC <- initFinderCache
  basic <- newUnitIndex
  let
    shared = hsc_env.hsc_unit_env.ue_index
    index = basic {
      readDatabases = \ logger unit cfg -> do
        dbs <- shared.readDatabases logger unit cfg
        pure (dbs ++ [staticUnitDatabase units])
    }
  pure (
    hsc_env {hsc_FC, hsc_unit_env = hsc_env.hsc_unit_env {ue_index = index}},
    Set.fromList (fst <$> units)
    )

readStaticUnits :: CachedBuildPlans -> IO [(UnitId, [ModuleName])]
readStaticUnits (CachedBuildPlans units) =
  for units \ CachedBuildPlan {name = JsonFs unit, build_plan} ->
    eitherDecodeFileStrict' (fromOsPath build_plan) >>= \case
      Right modules -> pure (unit, coerce (modules :: [JsonFs ModuleName]))
      Left err -> throwIO (userError err)

staticUnitDatabase :: [(UnitId, [ModuleName])] -> UnitDatabase UnitId
staticUnitDatabase units =
  UnitDatabase {
    unitDatabasePath = toOsPath "<static-dep-units>",
    unitDatabaseUnits = [staticUnitInfo unit modules | (unit, modules) <- units]
  }

staticUnitInfo :: UnitId -> [ModuleName] -> UnitInfo
staticUnitInfo unit modules =
  GenericUnitInfo {
    unitId = unit,
    unitInstanceOf = unit,
    unitInstantiations = [],
    unitPackageId = PackageId name,
    unitPackageName = PackageName name,
    unitPackageVersion = Version.makeVersion [0],
    unitComponentName = Nothing,
    unitAbiHash = ST.pack "",
    unitDepends = [],
    unitAbiDepends = [],
    unitImportDirs = [ST.pack "."],
    unitLibraries = [],
    unitExtDepLibsSys = [],
    unitExtDepLibsGhc = [],
    unitLibraryDirs = [],
    unitLibraryDynDirs = [],
    unitExtDepFrameworks = [],
    unitExtDepFrameworkDirs = [],
    unitLinkerOptions = [],
    unitCcOptions = [],
    unitIncludes = [],
    unitIncludeDirs = [],
    unitHaddockInterfaces = [],
    unitHaddockHTMLs = [],
    unitExposedModules = [(m, Nothing) | m <- modules],
    unitHiddenModules = [],
    unitIsIndefinite = False,
    unitIsExposed = False,
    unitIsTrusted = False
  }
  where
    UnitId name = unit

#else

prepareStaticSession _ _ =
  throwGhcExceptionIO (ProgramError "--dep-units-static requires UNIT_INDEX")

#endif
