{-# LANGUAGE CPP #-}

module Internal.State.Make where

import Control.Concurrent.MVar (MVar)
import Control.Monad.IO.Class (liftIO)
import GHC.Driver.Env (HscEnv (..))
import GHC.Runtime.Interpreter (Interp (..))
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import GHC.Types.Unique.DFM (plusUDFM)
import GHC.Unit.Env (HomeUnitEnv (..), HomeUnitGraph, UnitEnv (..), unitEnv_insert, unitEnv_lookup, unitEnv_union)
import GHC.Unit.Module.Graph (ModuleGraph)
import GHC.Utils.Outputable (doublePrec, text, (<+>))
import Internal.Log (Log, logd)

#if defined(MWB)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Unit.Module.Graph (ModuleGraphNode (..), mgModSummaries', mkModuleGraph, mkNodeKey)

#else

import GHC.Unit.Module.Graph (unionMG)

#endif

-- | Data extracted from 'HscEnv' for the purpose of persisting it across sessions.
--
-- While many parts of the session are either contained in mutable variables or trivially reinitialized, some components
-- must be handled explicitly: The module graph and home unit graph are pure fields that need to be shared, and the
-- interpreter state for TH execution is only initialized when the flags are parsed.
data MakeState =
  MakeState {
    -- | The module graph for a specific unit is computed in its metadata step, after which it's extracted and merged
    -- into the existing graph.
    moduleGraph :: ModuleGraph,

    -- | The unit environment for a specific unit is inserted into the shared home unit graph at the beginning of the
    -- metadata step, constructed from the dependency specifications provided by Buck.
    -- After compilation of a module, its 'HomeUnitInfo' is inserted into the home package table contained in its unit's
    -- unit environment.
    hug :: HomeUnitGraph,

    -- | While the interpreter state contains a mutable variable that would be shared across sessions, it isn't
    -- initialized properly until the first module compilation's flags have been parsed, so we store it in the shared
    -- state for consistency.
    interp :: Maybe Interp
  }

logMemStats :: String -> MVar Log -> IO ()
logMemStats step logVar = do
  s <- liftIO getRTSStats
  let logMem desc value = logd logVar (text (desc ++ ":") <+> doublePrec 2 (fromIntegral value / 1_000_000) <+> text "MB")
  logd logVar (text ("-------------- " ++ step))
  logMem "Mem in use" s.gc.gcdetails_mem_in_use_bytes
  logMem "Max mem in use" s.max_mem_in_use_bytes
  logMem "Max live bytes" s.max_live_bytes

-- | Restore the shared state used by both @computeMetadata@ and @compileHpt@ from the cache.
-- See 'loadCacheMakeCompile' for details.
loadState ::
  MVar Log ->
  HscEnv ->
  MakeState ->
  IO HscEnv
loadState logVar hsc_env state = do
  logMemStats "load state" logVar
  pure (restoreHug (restoreModuleGraph hsc_env))
  where
    restoreModuleGraph e = e {hsc_mod_graph = state.moduleGraph}

    restoreHug e = e {hsc_unit_env = e.hsc_unit_env {ue_home_unit_graph = state.hug}}

-- | Restore the shared state used by @compileHpt@ from the state, consisting of the module graph, the HPT, and the
-- loader state and symbol cache that's contained in 'Interp'.
-- The module graph is only modified by @computeMetadata@, so it will not be written back to the state after
-- compilation.
--
-- Managing 'Interp' is a bit difficult: The field 'hsc_interp' isn't initialized with everything else in 'newHscEnv',
-- but only after parsing the command line arguments in 'setTopSessionDynFlags', since it needs to know the Ways of the
-- session if an external interpreter is used.
-- Therefore we grab the 'Interp' from the session when the cached value is absent, which amounts to the first
-- compilation session of the build.
-- When the cached value is present, on the other hand, we instead restore it into the session, making all subsequent
-- sessions share the first one's 'Interp'.
-- Both fields of 'Interp' are 'MVar's, so the state is shared immediately and concurrently.
loadStateCompile ::
  MVar Log ->
  HscEnv ->
  MakeState ->
  IO (MakeState, HscEnv)
loadStateCompile logVar hsc_env0 state = do
  ensureInterp <$> loadState logVar hsc_env0 state
  where
    ensureInterp = maybe storeInterp restoreInterp state.interp

    storeInterp hsc_env = (state {interp = hsc_env.hsc_interp}, hsc_env)

    restoreInterp interp hsc_env = (state, hsc_env {hsc_interp = Just interp})

-- | Merge the given module graph into the cached graph.
-- In more recent versions of GHC, the function for merging graphs is not exposed anymore.
-- There was also some issue with node duplication, which is why this function is so convoluted.
storeModuleGraph :: ModuleGraph -> MakeState -> MakeState
storeModuleGraph new state =
#if defined(MWB)
  state {moduleGraph = merged}
  where
    !merged = merge state.moduleGraph

    merge old =
      mkModuleGraph (Map.elems (Map.unionWith mergeNodes oldMap newMap))
      where
        mergeNodes = \cases
          (ModuleNode oldDeps _) (ModuleNode newDeps summ) -> ModuleNode (mergeDeps oldDeps newDeps) summ
          _ newNode -> newNode

        mergeDeps oldDeps newDeps = Set.toList (Set.fromList oldDeps <> Set.fromList newDeps)

        oldMap = Map.fromList $ [(mkNodeKey n, n) | n <- mgModSummaries' old]

        newMap = Map.fromList $ [(mkNodeKey n, n) | n <- mgModSummaries' new]
#else
    state {moduleGraph = unionMG state.moduleGraph new}
#endif

-- | Extract the unit env of the currently active unit and store it in the cache.
-- This is used by the make mode worker after the metadata step has initialized the new unit.
insertUnitEnv :: HscEnv -> MakeState -> MakeState
insertUnitEnv hsc_env state =
  state {hug = update state.hug}
  where
    ue = unitEnv_lookup current hsc_env.hsc_unit_env.ue_home_unit_graph
    current = hsc_env.hsc_unit_env.ue_current_unit
    update = unitEnv_insert current ue

mergeHugs ::
  HomeUnitEnv ->
  HomeUnitEnv ->
  HomeUnitEnv
mergeHugs old new =
  new {homeUnitEnv_hpt = plusUDFM old.homeUnitEnv_hpt new.homeUnitEnv_hpt}

-- | Store the changes made to the HUG by @compileHpt@ in the state, which usually consists of adding a single
-- 'HomeModInfo'.
storeState ::
  MVar Log ->
  HscEnv ->
  MakeState ->
  IO MakeState
storeState logVar hsc_env state = do
  logMemStats "store make state" logVar
  let !new = hsc_env.hsc_unit_env.ue_home_unit_graph
      !hug = unitEnv_union mergeHugs state.hug new
  pure state {hug}
