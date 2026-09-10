{-# LANGUAGE CPP #-}

module Types.State.Make where

import Control.Concurrent.MVar (MVar)
import GHC (ModuleGraph, ModuleName, emptyMG)
import GHC.Data.Graph.Directed (Node)
import GHC.Runtime.Interpreter (Interp)
import GHC.Unit.Env (HomeUnitGraph)
import GHC.Unit.Module.Graph (ModuleGraphNode, NodeKey)
import GHC.Unit.Types (UnitId)
import Data.IntMap qualified as IM
import Data.Map.Strict qualified as M
import Data.Set qualified as S

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
  { keyIdxMap :: M.Map NodeKey Int,
    idxNodeMap :: IM.IntMap node,
    -- | key to (idx, graph node) pair.
    keyINodeMap :: M.Map NodeKey (Node Int node),
    idxKeyMap :: IM.IntMap NodeKey
  }

emptyKINMap :: KeyIndexNodeMap ModuleGraphNode
emptyKINMap = KIN
  { keyIdxMap = M.empty,
    idxNodeMap = IM.empty,
    keyINodeMap = M.empty,
    idxKeyMap = IM.empty
  }

data EModuleGraph = EModuleGraph
  { moduleGraph :: ModuleGraph,
    keyIndexNodeMap :: KeyIndexNodeMap ModuleGraphNode
  }

emptyEModuleGraph :: EModuleGraph
emptyEModuleGraph = EModuleGraph
  { moduleGraph = emptyMG,
    keyIndexNodeMap = emptyKINMap
  }

-- | Data extracted from 'HscEnv' for the purpose of persisting it across sessions.
--
-- While many parts of the session are either contained in mutable variables or trivially reinitialized, some components
-- must be handled explicitly: The module graph and home unit graph are pure fields that need to be shared, and the
-- interpreter state for TH execution is only initialized when the flags are parsed.
data MakeState =
  MakeState {
    -- | The module graph for a specific unit is computed in its metadata step, after which it's extracted and merged
    -- into the existing graph.
    moduleGraphState :: EModuleGraph,

    -- | moduleGraph nodes indexed by NodeKey.
    moduleGraphNodes :: M.Map NodeKey ModuleGraphNode,

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
    extraLib :: LibLoadState
  }
