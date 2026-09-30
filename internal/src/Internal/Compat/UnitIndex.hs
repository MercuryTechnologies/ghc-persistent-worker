{-# LANGUAGE CPP #-}

module Internal.Compat.UnitIndex where

import Data.Set (Set)
import GHC (DynFlags)
import GHC.Driver.Env (HscEnv (..))
import GHC.Platform (PlatformConstants)
import qualified GHC.Unit as GHC (initUnits)
import GHC.Unit (HomeUnit, UnitDatabase, UnitId, UnitState)
import GHC.Unit.Env (UnitEnv (..))

#if defined(UNIT_INDEX)

import GHC (Logger)
import GHC.Unit (UnitConfig)
import qualified GHC.Unit.State as GHC (readUnitDatabase)
import System.OsPath.Extra (OsPath)

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
import System.OsPath.Extra (fromOsPath)
#endif

initUnits ::
  HscEnv ->
  DynFlags ->
  Set UnitId ->
  IO ([UnitDatabase UnitId], UnitState, HomeUnit, Maybe PlatformConstants)
initUnits hsc_env dflags =
  GHC.initUnits hsc_env.hsc_logger dflags hsc_env.hsc_unit_env.ue_index Nothing

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
readUnitDatabase :: Logger -> UnitConfig -> OsPath -> IO (UnitDatabase UnitId)
readUnitDatabase logger cfg path = GHC.readUnitDatabase logger cfg (fromOsPath path)
#else
readUnitDatabase :: Logger -> UnitConfig -> OsPath -> IO (UnitDatabase UnitId)
readUnitDatabase = GHC.readUnitDatabase
#endif

#else

initUnits ::
  HscEnv ->
  DynFlags ->
  Set UnitId ->
  IO ([UnitDatabase UnitId], UnitState, HomeUnit, Maybe PlatformConstants)
initUnits hsc_env dflags =
  GHC.initUnits hsc_env.hsc_logger dflags Nothing

#endif
