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

initUnits ::
  HscEnv ->
  DynFlags ->
  Set UnitId ->
  IO ([UnitDatabase UnitId], UnitState, HomeUnit, Maybe PlatformConstants)
initUnits hsc_env dflags =
  GHC.initUnits hsc_env.hsc_logger dflags hsc_env.hsc_unit_env.ue_index Nothing

#else

initUnits ::
  HscEnv ->
  DynFlags ->
  Set UnitId ->
  IO ([UnitDatabase UnitId], UnitState, HomeUnit, Maybe PlatformConstants)
initUnits hsc_env dflags =
  GHC.initUnits hsc_env.hsc_logger dflags Nothing

#endif
