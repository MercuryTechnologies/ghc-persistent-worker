{-# LANGUAGE CPP #-}

module Internal.State.Make where

import Data.IntMap qualified as IM
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import GHC.Driver.Env (HscEnv (..))
import GHC.Unit.Env (UnitEnv (..))
import GHC.Unit.Home.Graph (UnitEnvGraph (..), unitEnv_insert, unitEnv_lookup)
import GHC.Unit.Module.Graph (ModuleGraph, ModuleGraphNode (..), NodeKey, mgModSummaries', mkNodeKey)
import Internal.State.Stats (logMemStats)
import Internal.State.UnitIndex (restoreUnitIndex)
import Types.Log (Logger)
import Types.State.Make (EModuleGraph (..), KeyIndexNodeMap (..), MakeState (..))

import Internal.Compat.ModuleGraph qualified as MG

-- | Restore the shared state used by both @computeMetadata@ and @compileHpt@ from the cache.
-- See 'loadCacheMakeCompile' for details.
loadState ::
  HscEnv ->
  MakeState ->
  HscEnv
loadState hsc_env state =
  restoreUnitIndex state (restoreHug (restoreModuleGraph hsc_env))
  where
#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
    restoreModuleGraph e = e {hsc_unit_env = e.hsc_unit_env {ue_module_graph = state.moduleGraphState.moduleGraph}}
#else
    restoreModuleGraph e = e {hsc_mod_graph = state.moduleGraphState.moduleGraph}
#endif

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
  HscEnv ->
  MakeState ->
  (MakeState, HscEnv)
loadStateCompile hsc_env0 state =
  ensureInterp (loadState hsc_env0 state)
  where
    ensureInterp = maybe storeInterp restoreInterp state.interp

    storeInterp hsc_env = (state {interp = hsc_env.hsc_interp}, hsc_env)

    restoreInterp interp hsc_env = (state, hsc_env {hsc_interp = Just interp})

-- | Merge the given nodes into the cached node index, leaving the derived 'moduleGraph' untouched.
--
-- In more recent versions of GHC, the function for merging graphs is not exposed anymore.
-- There was also some issue with node duplication, which is why this function is so convoluted.
mergeModuleGraphNodes ::
  [ModuleGraphNode] ->
  Map.Map NodeKey ModuleGraphNode ->
  Map.Map NodeKey ModuleGraphNode
mergeModuleGraphNodes new oldMap = merged
  where
    !merged = Map.unionWith mergeNodes oldMap newMap

    mergeNodes = \cases
      old@(ModuleNode _oldDeps _oldSumm) (ModuleNode _newDeps _newSumm) -> old
      _ newNode -> newNode

    newMap = Map.fromList $ [(mkNodeKey n, n) | n <- new]

mergeModuleGraph ::
  [(NodeKey, (Int, ModuleGraphNode))] ->
  EModuleGraph ->
  EModuleGraph
mergeModuleGraph kinodes egr =
  MG.extendReachIndex $ foldr MG.extendMG' egr kinodes

storeModuleGraphNodes :: [ModuleGraphNode] -> MakeState -> MakeState
storeModuleGraphNodes new state =
  state {
    moduleGraphState = egr',
    moduleGraphNodes = merged
  }
  where
    !merged = mergeModuleGraphNodes new state.moduleGraphNodes
    egr = state.moduleGraphState
    kinMap = egr.keyIndexNodeMap
    egr' = egr { keyIndexNodeMap = kinMap }

-- | Derive 'moduleGraph' from the node index.
--
-- This is @O(size of the index)@, so when a batch of units is restored it must be called once for the batch rather than
-- once per unit.
rebuildModuleGraph :: MakeState -> MakeState
rebuildModuleGraph !state =
  let old_egr = state.moduleGraphState
      KIN old_kmap old_inodes old_kss old_i2k old_reach = state.moduleGraphState.keyIndexNodeMap
      old_keys = Set.fromList (Map.keys old_kmap)
      old_n = Set.size old_keys
      all_nodes = state.moduleGraphNodes
      all_keys = Set.fromList (Map.keys all_nodes)
      all_n = Set.size all_keys
      delta_keys = all_keys `Set.difference` old_keys
      delta_kmap_list = zip (Set.toList delta_keys) [old_n + 1 .. all_n]
      delta_kmap = Map.fromList delta_kmap_list
      all_kmap = old_kmap `Map.union` delta_kmap
      delta_knodes = filter (\(k, _) -> k `Set.member` delta_keys) (Map.toList all_nodes)

      delta_kinodes_list :: [(NodeKey, (Int, ModuleGraphNode))]
      delta_kinodes_list = do
        (k, node) <- delta_knodes
        i <- maybeToList (Map.lookup k all_kmap)
        pure (k, (i, node))

      all_inodes = IM.union (IM.fromList (fmap snd delta_kinodes_list)) old_inodes

      delta_i2k = IM.fromList [ (i,k) | (k, (i, _)) <- delta_kinodes_list ]
      all_i2k = IM.union delta_i2k old_i2k

      -- BE CAREFUL old_kss and old_reach
      newKIN0 = KIN all_kmap all_inodes old_kss all_i2k old_reach
      new_egr1 = mergeModuleGraph delta_kinodes_list (old_egr {keyIndexNodeMap = newKIN0})
      newKIN1 = keyIndexNodeMap new_egr1
      new_egr = new_egr1 { keyIndexNodeMap = newKIN1 }
   in state {
     moduleGraphState = new_egr
   }

-- | Merge the given module graph into the cached graph and derive 'moduleGraph' immediately.
storeModuleGraph :: ModuleGraph -> MakeState -> MakeState
storeModuleGraph new =
  rebuildModuleGraph . storeModuleGraphNodes (mgModSummaries' new)

-- | Extract the unit env of the currently active unit and store it in the cache.
-- This is used by the make mode worker after the metadata step has initialized the new unit.
insertUnitEnv :: HscEnv -> MakeState -> MakeState
insertUnitEnv hsc_env state =
  state {hug = update state.hug}
  where
    ue = unitEnv_lookup current hsc_env.hsc_unit_env.ue_home_unit_graph
    current = hsc_env.hsc_unit_env.ue_current_unit
    update = unitEnv_insert current ue

-- | Store the changes made to the HUG by @compileHpt@ in the state, which usually consists of adding a single
-- 'HomeModInfo'.
storeState ::
  Logger ->
  HscEnv ->
  MakeState ->
  IO MakeState
storeState logger hsc_env state = do
  logMemStats "store make state" logger
  pure state {hug}
  where
    !hug = UnitEnvGraph (new <> old)

    UnitEnvGraph !new = hsc_env.hsc_unit_env.ue_home_unit_graph

    UnitEnvGraph !old = state.hug
