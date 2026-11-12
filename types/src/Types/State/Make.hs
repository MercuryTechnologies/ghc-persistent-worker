{-# LANGUAGE CPP #-}

module Types.State.Make where

import Control.Concurrent.MVar (MVar)
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import GHC (ModuleGraph, ModuleName, emptyMG)
import GHC.Data.Graph.Directed (Node)
import GHC.Linker.Types (Linkable)
import GHC.Runtime.Interpreter (Interp)
import GHC.Unit.Env (HomeUnitGraph)
import GHC.Unit.Module.Graph (ModuleGraphNode, NodeKey)
import GHC.Unit.Types (Module, UnitId)

#if defined(UNIT_INDEX)

import GHC.Unit.State (UnitIndex)

#else

data UnitIndex = UnitIndex

#endif

type LibName = String

-- | Currently requested and loaded dynamic libraries
--   which are being loaded via direct loadDLL calls.
--   Loaded libraries are tracked and loading is done only once.
data LibLoadState =
  LibLoadState {
    requested :: M.Map UnitId ([FilePath], [LibName]),
    loaded :: S.Set LibName
  }

emptyLibLoadState :: LibLoadState
emptyLibLoadState = LibLoadState
  { requested = M.empty,
    loaded = S.empty
  }

-- | Maps among unique key, graph idx and graph node content.
-- This is actual content of graph and integerization of nodes
-- for efficient query.
-- In our case, node = ModuleGraphNode
-- Note that Node Int inode = SummaryNode
data KeyIndexNodeMap node = KIN
  { -- | Index assigned to each node already folded into 'moduleGraph'.
    -- Its key set doubles as the record of what the graph currently holds,
    -- and is what @rebuildModuleGraph@ diffs against
    -- 'MakeState.moduleGraphNodes' to find the nodes still to add.
    keyIdxMap :: M.Map NodeKey Int,
    -- | Node content by index.
    idxNodeMap :: IM.IntMap node,
    -- | key to (idx, graph node) pair.
    keyINodeMap :: M.Map NodeKey (Node Int node),
    -- | Inverse of the key/index mapping in 'keyIdxMap'.
    idxKeyMap :: IM.IntMap NodeKey,
    -- | Transitive reachability per node index. Extended once per merge by
    -- @extendReachIndex@, not once per node.
    reachabilityMap :: IM.IntMap IS.IntSet,
    -- | For GHC 9.14's loop graph: the indices of the hs-boot nodes and of
    -- the modules they belong to, the nodes that folding hs-boot files
    -- changes. A node whose reachable set has none of them reaches the same
    -- nodes in the loop graph, and shares its set.
    foldedIdxs :: IS.IntSet
  }

emptyKINMap :: KeyIndexNodeMap ModuleGraphNode
emptyKINMap = KIN
  { keyIdxMap = M.empty,
    idxNodeMap = IM.empty,
    keyINodeMap = M.empty,
    idxKeyMap = IM.empty,
    reachabilityMap = IM.empty,
    foldedIdxs = IS.empty
  }

-- | A 'ModuleGraph' together with the integer index used to query it.
--
-- Both fields are derived rather than authoritative: they are rebuilt from
-- 'MakeState.moduleGraphNodes' by @rebuildModuleGraph@, and the two are
-- kept consistent with each other.
data EModuleGraph = EModuleGraph
  { moduleGraph :: ModuleGraph,
    keyIndexNodeMap :: KeyIndexNodeMap ModuleGraphNode
  }

emptyEModuleGraph :: EModuleGraph
emptyEModuleGraph = EModuleGraph
  { moduleGraph = emptyMG,
    keyIndexNodeMap = emptyKINMap
  }

-- | Loader state tracking metadata for a single 'Linkable'.
data BcoCacheEntry =
  BcoCacheEntry {
    linkable :: Linkable,
    -- | A measure for the age of this entry, representing the value of 'bcoAccessCounter' at the time it was last used.
    lastAccess :: Int,
    -- | The number of BCOs contained in the linkable, used as a proxy for size.
    size :: Int
  }

-- | Per-module GHC flags (e.g. from @per_module_plugins@) for fixed nodes
type FixedNodeFlags = M.Map NodeKey (NonEmpty String)

-- | Data extracted from 'HscEnv' for the purpose of persisting it across sessions.
--
-- While many parts of the session are either contained in mutable variables or trivially reinitialized, some components
-- must be handled explicitly: The module graph and home unit graph are pure fields that need to be shared, and the
-- interpreter state for TH execution is only initialized when the flags are parsed.
data MakeState =
  MakeState {
    -- | The module graph for a specific unit is computed in its metadata step, after which it's extracted and merged
    -- into the existing graph.
    --
    -- Derived from 'moduleGraphNodes'; see that field for the invariant
    -- relating the two.
    moduleGraphState :: EModuleGraph,

    -- | Every module graph node seen so far in this session, keyed by
    -- 'NodeKey'. This is the authoritative collection, from which
    -- 'moduleGraphState' is derived.
    --
    -- Units add their nodes here during the metadata step via
    -- @storeModuleGraphNodes@, and @rebuildModuleGraph@ then folds whatever
    -- is not yet recorded in @moduleGraphState.keyIndexNodeMap.keyIdxMap@
    -- into the graph. So between those two calls this map is a superset of
    -- the graph's contents, and immediately after a rebuild the two hold
    -- exactly the same keys.
    --
    -- Nodes are only ever added: @mergeModuleGraphNodes@ unions into this
    -- map and nothing removes from it. A module deleted from a package
    -- therefore leaves a stale node behind for the lifetime of the worker;
    -- there is currently no invalidation path, and the map grows
    -- monotonically.
    moduleGraphNodes :: M.Map NodeKey ModuleGraphNode,

    -- | Per-module GHC flags (e.g. from @per_module_plugins@) for modules that
    -- were restored as 'GHC.Unit.Module.Graph.ModuleNodeFixed' stubs rather
    -- than 'GHC.Unit.Module.Graph.ModuleNodeCompile' nodes. Since building a
    -- fixed node skips flag application, the flags are stashed here and applied
    -- when (and only if) a compile action needs a full 'GHC.ModSummary' for
    -- that module.
    fixedNodeFlags :: FixedNodeFlags,

    -- | The unit environment for a specific unit is inserted into the shared home unit graph at the beginning of the
    -- metadata step, constructed from the dependency specifications provided by Buck.
    -- After compilation of a module, its 'HomeUnitInfo' is inserted into the home package table contained in its unit's
    -- unit environment.
    hug :: HomeUnitGraph,

    -- | While the interpreter state contains a mutable variable that would be shared across sessions, it isn't
    -- initialized properly until the first module compilation's flags have been parsed, so we store it in the shared
    -- state for consistency.
    interp :: Maybe Interp,

    unitIndex :: UnitIndex,

    bcoLoadState :: M.Map ModuleName (MVar ()),

    -- | Unit-level extra native library dependencies are loaded by checking in LibLoadState explicitly.
    extraLib :: LibLoadState,

    -- | Track loader state entries for unloading.
    bcoCache :: M.Map Module BcoCacheEntry,

    -- | Monotonic counter bumped every time bytecode dependencies are loaded, used as the age of loader state entries.
    bcoAccessCounter :: Int
  }
