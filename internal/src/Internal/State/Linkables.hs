{-# LANGUAGE CPP #-}

module Internal.State.Linkables where

import Control.Concurrent (MVar)
import GHC.Driver.Env.Types (HscEnv)
import Types.Log (Logger (..))
import Types.State (WorkerState)

#if defined(LINKABLES)

import Control.Concurrent (modifyMVar)
import Data.Foldable (traverse_)
import GHC (Module)
import GHC.Driver.Config.Finder (initFinderOpts)
import GHC.Driver.Env (hscInterp, hsc_home_unit, hsc_units)
import qualified GHC.Driver.Env.Types as GHC
import GHC.Driver.Env.Types (HscEnv (..), LinkDeps, Linkables (Linkables))
import GHC.Linker.Deps (LinkDepsOpts)
import GHC.Linker.Loader (initLinkDepsOpts)
import GHC.Linker.Types (Linkable, LoaderState)
import GHC.Runtime.Interpreter (Interp)
import GHC.Types.SrcLoc (SrcSpan)
import GHC.Unit (moduleUnitId)
import GHC.Unit.Finder (findExactModule)
import GHC.Unit.Finder.Types (FinderCache, FinderOpts, InstalledFindResult (..))
import GHC.Unit.Home.Graph (HomeUnitEnv (..), HomeUnitGraph, UnitEnvGraph, unitEnv_lookup_maybe)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomeModLinkable (..))
import GHC.Unit.Home.PackageTable (addHomeModInfoToHpt)
import GHC.Unit.Module.Location (ModLocation)
import GHC.Unit.Module.ModIface (mi_module)
import GHC.Unit.Types (toUnitId)
import GHC.Utils.Outputable (parens, ppr, (<+>))
import Internal.Cache.Hpt (loadCachedByteCodeFrom)
import Internal.Compat.LinkDeps (getLinkDeps)
import Internal.Error (workerErrorIO)
import Language.Haskell.Syntax.ImpExp (IsBootInterface (..))
import Types.State (WorkerState (..))
import Types.State.Make (MakeState (..))

requireLocation ::
  HscEnv ->
  Module ->
  InstalledFindResult ->
  IO ModLocation
requireLocation hsc_env module_ = \case
  InstalledFound location -> pure location
  InstalledNoPackage {} -> emitError "no package"
  InstalledNotFound {} -> emitError "not found"
  where
    emitError msg =
      workerErrorIO hsc_env ("Lazy bytecode loader could not find location of" <+> ppr module_ <+> parens msg)

withFinder :: HscEnv -> HomeUnitGraph -> (FinderCache -> FinderOpts -> UnitEnvGraph FinderOpts -> a) -> a
withFinder hsc_env hug f =
  f hsc_env.hsc_FC (initFinderOpts hsc_env.hsc_dflags) other_fopts
  where
    other_fopts = initFinderOpts . homeUnitEnv_dflags <$> hug

lazyLoadByteCode ::
  Logger ->
  MVar WorkerState ->
  HscEnv ->
  HomeModInfo ->
  IO (Maybe Linkable)
lazyLoadByteCode logger stateVar hsc_env hmi = do
  logger.debugD ("Loading lazy bytecode for " <+> ppr module_)
  modifyMVar stateVar \ state -> do
    result <- withFinder hsc_env state.make.hug findExactModule (hsc_units hsc_env) (Just (hsc_home_unit hsc_env)) (toUnitId <$> module_) NotBoot
    location <- requireLocation hsc_env module_ result
    loadCachedByteCodeFrom hsc_env location (hm_iface hmi) (hm_details hmi) >>= \case
      Just bytecode -> do
        let iface = hm_iface hmi
            new = hmi {hm_iface = iface, hm_linkable = hmi.hm_linkable {homeMod_bytecode = Just bytecode}}
        traverse_ (insertIntoHpt new) (unitEnv_lookup_maybe (moduleUnitId (mi_module iface)) state.make.hug)
        pure (state, Just bytecode)
      Nothing -> pure (state, Nothing)
  where
    insertIntoHpt new hue = addHomeModInfoToHpt new (homeUnitEnv_hpt hue)

    module_ = mi_module hmi.hm_iface

linkablesResolve ::
  Logger ->
  MVar WorkerState ->
  HscEnv ->
  LinkDepsOpts ->
  Interp ->
  LoaderState ->
  SrcSpan ->
  [Module] ->
  IO LinkDeps
linkablesResolve logger stateVar hsc_env o i l s m = do
  getLinkDeps o i l (lazyLoadByteCode logger stateVar hsc_env) s m

linkablesSelect :: LinkDeps -> IO LinkDeps
linkablesSelect deps = do
  pure deps

newLinkables ::
  Logger ->
  MVar WorkerState ->
  HscEnv ->
  LoaderState ->
  IO Linkables
newLinkables logger stateVar hsc_env pls = do
  pure Linkables {
    linkablesResolve = linkablesResolve logger stateVar hsc_env (initLinkDepsOpts hsc_env) (hscInterp hsc_env) pls,
    linkablesSelect
  }

#endif

installLinkables :: Logger -> MVar WorkerState -> HscEnv -> HscEnv

#if defined(LINKABLES)

installLinkables logger stateVar hsc_env =
  hsc_env {hsc_linkables = newLinkables logger stateVar}

#else

installLinkables _ _ = id

#endif
