{-# LANGUAGE CPP #-}

module Internal.Compat.GHC914 where

import GHC (Located, ModIface, ModSummary, ModuleGraph, ModuleName, PkgQual)
import GHC.Driver.Env (HscEnv (..))
import qualified GHC.Driver.Session as GHC
import GHC.Iface.Syntax (IfaceBindingX, IfaceMaybeRhs, IfaceTopBndrInfo)
import GHC.LanguageExtensions (Extension)
import GHC.Unit.Module.Graph (ModuleGraphNode, NodeKey)
import GHC.Unit.Types (UnitId)

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

import Data.Functor ((<&>))
import qualified Data.Set as Set
import GHC.Driver.Env (hsc_units)
import GHC.Driver.Flags (OnOff (..))
import GHC.Unit.Env (UnitEnv (..))
import GHC.Unit.Home.Graph (unitEnv_keys)
import GHC.Unit.Info (GenericUnitInfo (..))
import GHC.Unit.Module.Graph (
  ModuleGraphNode (..),
  ModuleNodeEdge,
  ModuleNodeInfo (..),
  NodeKey (..),
  edgeTargetKey,
  mgMapM,
  mkNormalEdge,
  )
import GHC.Unit.State (UnitState (..), lookupUnitId)
import GHC.Unit.Types (toUnitId)
import GHC.Unit.Module.ModIface (IfaceSimplifiedCore (..), set_mi_simplified_core)
import GHC.Unit.Module.WholeCoreBindings (emptyIfaceForeign)

hscModuleGraph :: HscEnv -> ModuleGraph
hscModuleGraph hsc_env = hsc_env.hsc_unit_env.ue_module_graph

hscSetModuleGraph :: ModuleGraph -> HscEnv -> HscEnv
hscSetModuleGraph ue_module_graph hsc_env = hsc_env {hsc_unit_env = hsc_env.hsc_unit_env {ue_module_graph}}

moduleNodeEdge :: NodeKey -> ModuleNodeEdge
moduleNodeEdge = mkNormalEdge

-- | A module's edge to an external unit it imports, at the normal level.
externalUnitEdges :: UnitId -> [ModuleNodeEdge]
externalUnitEdges unit = [mkNormalEdge (NodeKey_ExternalUnit unit)]

edgeTarget :: ModuleNodeEdge -> NodeKey
edgeTarget = edgeTargetKey

textualImports :: (a, PkgQual, Located ModuleName) -> (PkgQual, Located ModuleName)
textualImports (_, pkg, name) = (pkg, name)

setExtraDecls :: Maybe [IfaceBindingX IfaceMaybeRhs IfaceTopBndrInfo] -> ModIface -> ModIface
setExtraDecls new =
  set_mi_simplified_core $ new <&> \ mi_sc_extra_decls ->
    IfaceSimplifiedCore {mi_sc_foreign = emptyIfaceForeign, ..}

-- | Like 'mapMG', but monadic.
mapMGM :: (ModSummary -> IO ModSummary) -> ModuleGraph -> IO ModuleGraph
mapMGM f = mgMapM $ \mni -> case mni of
  ModuleNodeCompile ms -> ModuleNodeCompile <$> f ms
  _ -> pure mni

impliedXFlags :: [(Extension, Bool, Extension)]
impliedXFlags =
  [compat ext dep | (ext, dep) <- GHC.impliedXFlags]
  where
    compat ext = \case
      On dep -> (ext, True, dep)
      Off dep -> (ext, False, dep)

-- | A @UnitNode@ for each external unit the active unit depends on, and for each unit those depend on, transitively.
--
-- GHC's downsweep adds these nodes for the external units a unit's modules import (@loopUnit@), but a unit restored from
-- the cache has no downsweep. The explicit dependencies of the unit, from its @-package@ flags, are a superset of the
-- units its modules import, and the dependencies come from the package database, as @loopUnit@ takes them. Home units
-- are left out, as they are not external.
externalUnitNodes :: HscEnv -> [ModuleGraphNode]
externalUnitNodes hsc_env =
  go Set.empty [toUnitId unit | (unit, _) <- explicitUnits units]
  where
    units = hsc_units hsc_env

    home = unitEnv_keys hsc_env.hsc_unit_env.ue_home_unit_graph

    go seen = \case
      [] -> []
      uid : rest
        | Set.member uid seen || Set.member uid home -> go seen rest
        | Just info <- lookupUnitId units uid ->
          UnitNode (unitDepends info) uid : go (Set.insert uid seen) (unitDepends info <> rest)
        | otherwise -> go (Set.insert uid seen) rest

#else

import Control.Monad (forM)
import GHC.Unit.Module.Graph (ModuleGraph (..), ModuleGraphNode (..), ModuleNodeInfo (..))

#if defined(MWB)

import GHC.Unit.Module.ModIface (set_mi_extra_decls)

setExtraDecls :: Maybe [IfaceBindingX IfaceMaybeRhs IfaceTopBndrInfo] -> ModIface -> ModIface
setExtraDecls = set_mi_extra_decls

#else

import GHC (mi_extra_decls)

setExtraDecls :: Maybe [IfaceBindingX IfaceMaybeRhs IfaceTopBndrInfo] -> ModIface -> ModIface
setExtraDecls mi_extra_decls iface = iface {mi_extra_decls}

#endif

hscModuleGraph :: HscEnv -> ModuleGraph
hscModuleGraph hsc_env = hsc_env.hsc_mod_graph

hscSetModuleGraph :: ModuleGraph -> HscEnv -> HscEnv
hscSetModuleGraph hsc_mod_graph hsc_env = hsc_env {hsc_mod_graph}

moduleNodeEdge :: NodeKey -> NodeKey
moduleNodeEdge = id

-- | GHC 9.10's module graph has no edges to external units.
externalUnitEdges :: UnitId -> [NodeKey]
externalUnitEdges _ = []

edgeTarget :: NodeKey -> NodeKey
edgeTarget = id

textualImports :: (PkgQual, Located ModuleName) -> (PkgQual, Located ModuleName)
textualImports = id

mgMapM :: (ModuleNodeInfo -> IO ModuleNodeInfo) -> ModuleGraph -> IO ModuleGraph
mgMapM f mg = do
  mgns <- forM (mg_mss mg) $ \mgn -> case mgn of
      ModuleNode deps mni  -> ModuleNode deps <$> f mni
      _ -> pure mgn
  pure mg { mg_mss = mgns }

-- | Like 'mapMG', but monadic.
mapMGM :: (ModSummary -> IO ModSummary) -> ModuleGraph -> IO ModuleGraph
mapMGM f = mgMapM $ \mni -> case mni of
  ModuleNodeCompile ms -> ModuleNodeCompile <$> f ms
  _ -> pure mni

impliedXFlags :: [(Extension, Bool, Extension)]
impliedXFlags = GHC.impliedXFlags

-- | GHC 9.10's module graph has no nodes for external units.
externalUnitNodes :: HscEnv -> [ModuleGraphNode]
externalUnitNodes _ = []

#endif
