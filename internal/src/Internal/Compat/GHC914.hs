{-# LANGUAGE CPP #-}

module Internal.Compat.GHC914 where

import GHC (Located, ModIface, ModSummary, ModuleGraph, ModuleName, PkgQual)
import GHC.Driver.Env (HscEnv (..))
import GHC.Iface.Syntax (IfaceBindingX, IfaceMaybeRhs, IfaceTopBndrInfo)
import GHC.Unit.Module.Graph (ModuleGraphNode(..), NodeKey)

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

import Data.Functor ((<&>))
import GHC.Unit.Env (UnitEnv (..))
import GHC.Unit.Module.Graph
    (ModuleNodeEdge, ModuleNodeInfo(..), edgeTargetKey, mkNormalEdge, mgMapM)
import GHC.Unit.Module.ModIface (IfaceSimplifiedCore (..), set_mi_simplified_core)
import GHC.Unit.Module.WholeCoreBindings (emptyIfaceForeign)

hscModuleGraph :: HscEnv -> ModuleGraph
hscModuleGraph hsc_env = hsc_env.hsc_unit_env.ue_module_graph

hscSetModuleGraph :: ModuleGraph -> HscEnv -> HscEnv
hscSetModuleGraph ue_module_graph hsc_env = hsc_env {hsc_unit_env = hsc_env.hsc_unit_env {ue_module_graph}}

moduleNodeEdge :: NodeKey -> ModuleNodeEdge
moduleNodeEdge = mkNormalEdge

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

#else

import Control.Monad (forM)
import GHC.Unit.Module.Graph (ModuleGraph(..))

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

edgeTarget :: NodeKey -> NodeKey
edgeTarget = id

textualImports :: (PkgQual, Located ModuleName) -> (PkgQual, Located ModuleName)
textualImports = id

-- | Like 'mapMG', but monadic.
mapMGM :: (ModSummary -> IO ModSummary) -> ModuleGraph -> IO ModuleGraph
mapMGM f mg = do
  mgns <- forM (mg_mss mg) $ \mgn -> case mgn of
      ModuleNode deps ms  -> ModuleNode deps <$> f ms
      _ -> pure mgn
  pure mg { mg_mss = mgns }

#endif
