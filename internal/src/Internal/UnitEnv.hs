{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
#define RECENT (MIN_VERSION_GLASGOW_HASKELL(9,13,0,0) || defined(MWB) || defined(MWB_2025_10))

module Internal.UnitEnv where

import GHC (HscEnv)
import GHC.Unit (ModuleName, UnitId)
import GHC.Unit.Env (HomeUnitGraph)
import GHC.Unit.Home.ModInfo (HomeModInfo)

#if RECENT

import Data.Foldable (for_)
import qualified Data.Map.Merge.Strict as Map
import Data.Map.Merge.Strict (preserveMissing, zipWithMatched)
import Data.Maybe (isJust)
import GHC.Driver.Env (hscInsertHPT)
import GHC.Unit.Home.Graph (UnitEnvGraph (..), unitEnv_lookup_maybe)
import qualified GHC.Unit.Home.PackageTable as GHC
import GHC.Unit.Home.PackageTable (HomePackageTable (..))

#else

import GHC.Driver.Env (hscUpdateHPT, hscUpdateHUG)
import GHC.Types.Unique.DFM (addToUDFM, plusUDFM)
import qualified GHC.Unit.Env as GHC
import GHC.Unit.Env (HomeUnitEnv (..), UnitEnvGraph, addHomeModInfoToHug)
import qualified GHC.Unit.Home.ModInfo as GHC
import GHC.Unit.Home.ModInfo (HomePackageTable)

#endif

#if RECENT

unitEnv_member :: UnitId -> UnitEnvGraph v -> Bool
unitEnv_member k = isJust . unitEnv_lookup_maybe k

unitEnv_union :: (a -> a -> a) -> UnitEnvGraph a -> UnitEnvGraph a -> UnitEnvGraph a
unitEnv_union f (UnitEnvGraph env1) (UnitEnvGraph env2) =
  UnitEnvGraph (Map.merge preserveMissing preserveMissing (zipWithMatched (const f)) env1 env2)

mergeHugs :: HomeUnitGraph -> HomeUnitGraph -> HomeUnitGraph
mergeHugs = unitEnv_union (const id)

lookupHpt :: HomePackageTable -> ModuleName -> IO (Maybe HomeModInfo)
lookupHpt = GHC.lookupHpt

addHomeModInfoToHpt :: HscEnv -> ModuleName -> HomeModInfo -> HomePackageTable -> IO HscEnv
addHomeModInfoToHpt hsc_env _ hmi hpt  = hsc_env <$ GHC.addHomeModInfoToHpt hmi hpt

emptyHomePackageTable :: IO HomePackageTable
emptyHomePackageTable = GHC.emptyHomePackageTable

-- | Insert a compilation result into the current unit's home package table, as it is done by upsweep.
addDepsToHscEnv :: [HomeModInfo] -> HscEnv -> IO HscEnv
addDepsToHscEnv deps hsc_env = do
  for_ deps \ dep -> hscInsertHPT dep hsc_env
  pure hsc_env

#else

unitEnv_member :: UnitId -> UnitEnvGraph v -> Bool
unitEnv_member = GHC.unitEnv_member

lookupHpt :: HomePackageTable -> ModuleName -> IO (Maybe HomeModInfo)
lookupHpt hpt = pure . GHC.lookupHpt hpt

addHomeModInfoToHpt :: HscEnv -> ModuleName -> HomeModInfo -> HomePackageTable -> IO HscEnv
addHomeModInfoToHpt hsc_env name hmi hpt =
  pure (hscUpdateHPT (const (addToUDFM hpt name hmi)) hsc_env)

emptyHomePackageTable :: IO HomePackageTable
emptyHomePackageTable = pure GHC.emptyHomePackageTable

addDepsToHscEnv :: [HomeModInfo] -> HscEnv -> IO HscEnv
addDepsToHscEnv deps hsc_env = pure (hscUpdateHUG (\hug -> foldr addHomeModInfoToHug hug deps) hsc_env)

mergeHpts :: HomeUnitEnv -> HomeUnitEnv -> HomeUnitEnv
mergeHpts old new = new {homeUnitEnv_hpt = plusUDFM old.homeUnitEnv_hpt new.homeUnitEnv_hpt}

mergeHugs :: HomeUnitGraph -> HomeUnitGraph -> HomeUnitGraph
mergeHugs old new = GHC.unitEnv_union mergeHpts old new

#endif
