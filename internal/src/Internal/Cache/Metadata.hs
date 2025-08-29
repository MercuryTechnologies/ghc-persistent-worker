module Internal.Cache.Metadata where

import Control.Monad.IO.Class (liftIO)
import GHC (DynFlags (..), Logger)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Session (updatePlatformConstants)
import GHC.Unit (HomeUnit, UnitDatabase, UnitId, UnitState, initUnits)
import GHC.Unit.Env (HomeUnitEnv (..), UnitEnv (..), unitEnv_insert, unitEnv_keys, updateHug)
import GHC.Unit.Home.ModInfo (emptyHomePackageTable)

-- | Add a fresh 'HomeUnitEnv' to the home unit graph using the supplied unit state and dependencies.
insertHomeUnit ::
  UnitId ->
  DynFlags ->
  [UnitDatabase UnitId] ->
  UnitState ->
  HomeUnit ->
  UnitEnv ->
  UnitEnv
insertHomeUnit unit dflags dbs unit_state home_unit unit_env =
  (updateHug (unitEnv_insert unit hue) unit_env) {
    ue_platform = targetPlatform dflags,
    ue_namever = ghcNameVersion dflags
  }
  where
    hue = HomeUnitEnv {
      homeUnitEnv_units = unit_state,
      homeUnitEnv_unit_dbs = Just dbs,
      homeUnitEnv_dflags = dflags,
      homeUnitEnv_hpt = emptyHomePackageTable,
      homeUnitEnv_home_unit = Just home_unit
    }

-- | Create a new home unit using the supplied 'DynFlags'.
initHomeUnit :: DynFlags -> Logger -> UnitId -> UnitEnv -> IO UnitEnv
initHomeUnit dflags0 logger unit unit_env = do
  (dbs, unit_state, home_unit, mconstants) <- initUnits logger dflags0 Nothing allUnitIds
  dflags1 <- updatePlatformConstants dflags0 mconstants
  pure (insertHomeUnit unit dflags1 dbs unit_state home_unit unit_env)
  where
    allUnitIds = unitEnv_keys (ue_home_unit_graph unit_env)

-- | Add a new home unit to the given session using the provided 'DynFlags'.
-- The flags have been constructed from Buck CLI args passed to the metadata step, which, crucially, contain the package
-- DB arguments for dependencies.
addHomeUnitTo :: HscEnv -> DynFlags -> IO (HscEnv, UnitId)
addHomeUnitTo hsc_env dflags = do
  unit_env <- liftIO $ initHomeUnit dflags hsc_env.hsc_logger unit hsc_env.hsc_unit_env
  pure (hsc_env {hsc_unit_env = unit_env}, unit)
  where
    unit = dflags.homeUnitId_

