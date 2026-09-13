{-# LANGUAGE CPP #-}

module Internal.State.Linkables where

import Control.Concurrent (MVar)
import GHC.Driver.Env.Types (HscEnv)
import Types.Log (Logger (..))
import Types.State (WorkerState)

#if defined(LINKABLES)

import Control.Concurrent (modifyMVar)
import Control.Monad (foldM)
import Data.Foldable (for_, traverse_)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import GHC (Module)
import GHC.Driver.Config.Finder (initFinderOpts)
import GHC.Driver.DynFlags (targetPlatform)
import GHC.Driver.Env (hscInterp, hsc_home_unit, hsc_units)
import GHC.Driver.Env.Types (HscEnv (..))
import GHC.Linker.Deps (LinkDepsOpts, LinkModule (..), ldUseByteCode, resolveLinkDeps, selectLinkDeps)
import GHC.Linker.Loader (initLinkDepsOpts)
import qualified GHC.Linker.Types
import GHC.Linker.Types (LinkDeps, Linkable (..), Linkables (Linkables), LoaderState)
import GHC.Platform (platformSOName)
import GHC.Runtime.Interpreter (Interp, loadDLL)
import GHC.Types.SrcLoc (SrcSpan)
import GHC.Types.Unique.DSet (UniqDSet)
import GHC.Unit (UnitId, moduleUnitId)
import GHC.Unit.Finder (findExactModule)
import GHC.Unit.Finder.Types (FinderCache, FinderOpts, InstalledFindResult (..))
import GHC.Unit.Home.Graph (HomeUnitEnv (..), HomeUnitGraph, UnitEnvGraph, unitEnv_lookup_maybe)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomeModLinkable (..))
import GHC.Unit.Home.PackageTable (addHomeModInfoToHpt)
import GHC.Unit.Module.Location (ModLocation)
import GHC.Unit.Module.ModIface (mi_module)
import GHC.Unit.Types (toUnitId)
import GHC.Utils.Outputable (parens, ppr, text, (<+>))
import Internal.Cache.Hpt (loadCachedByteCodeFrom)
import Internal.Error (workerErrorIO)
import Internal.State (modifyMakeState)
import Language.Haskell.Syntax.ImpExp (IsBootInterface (..))
import System.Directory (findFile)
import Types.State (WorkerState (..))
import Types.State.Make (LibLoadState (..), MakeState (..))


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

loadDLL_ :: HscEnv -> Interp -> [FilePath] -> String -> IO ()
loadDLL_ hsc_env interp lib_paths lib = do
  let dflags = hsc_dflags hsc_env
      platform = targetPlatform dflags
      so_name = platformSOName platform lib
  mb_so_file  <- findFile lib_paths so_name
  case mb_so_file of
    Nothing -> emitError ("Library not found: " <+> text so_name)
    Just so_file -> do
      e <- loadDLL interp so_file
      case e of
        Left err -> emitError (text err)
        Right _ -> pure ()
  where
    emitError msg =
      workerErrorIO hsc_env ("Loading DLL error:" <+> msg)

addLazyByteCode ::
  Logger ->
  MVar WorkerState ->
  HscEnv ->
  LinkModule ->
  IO LinkModule
addLazyByteCode logger stateVar hsc_env = \case
  LinkHomeModule hmi@HomeModInfo {hm_linkable = HomeModLinkable {homeMod_bytecode = Nothing}} -> do
    homeMod_bytecode <- lazyLoadByteCode logger stateVar hsc_env hmi
    pure (LinkHomeModule hmi {hm_linkable = hmi.hm_linkable {homeMod_bytecode}})
  lm -> pure lm

ensureLibraries :: MVar WorkerState -> HscEnv -> Interp -> [LinkModule] -> IO ()
ensureLibraries stateVar hsc_env interp deps =
  for_ unitIds \unit_id ->
    modifyMakeState stateVar \make -> do
      let m = make.extraLib.requested
      case M.lookup unit_id m of
        Nothing -> pure (make, ())
        Just (lib_paths, libs) -> do
          let load loaded lib
                | lib `S.member` loaded = pure loaded
                | otherwise = loadDLL_ hsc_env interp lib_paths lib >> pure (S.insert lib loaded)
          loaded' <- foldM load make.extraLib.loaded libs
          pure (make {extraLib = LibLoadState m loaded'}, ())
  where
    unitIds = [moduleUnitId (mi_module hm_iface) | LinkHomeModule HomeModInfo {hm_iface} <- deps]

-- | Wrap the native 'resolveLinkDeps' to lazily load bytecode for all home modules that lack it.
linkablesResolve ::
  Logger ->
  MVar WorkerState ->
  HscEnv ->
  LinkDepsOpts ->
  LoaderState ->
  SrcSpan ->
  [Module] ->
  IO ([Linkable], [LinkModule], UniqDSet UnitId, [UnitId])
linkablesResolve logger stateVar hsc_env opts pls srcSpan mods = do
  (loaded, needed, allUnits, neededUnits) <- resolveLinkDeps opts pls srcSpan mods
  neededWithLazy <-
    if ldUseByteCode opts
    then traverse (addLazyByteCode logger stateVar hsc_env) needed
    else pure needed
  ensureLibraries stateVar hsc_env (hscInterp hsc_env) neededWithLazy
  pure (loaded, neededWithLazy, allUnits, neededUnits)

newLinkables ::
  Logger ->
  MVar WorkerState ->
  HscEnv ->
  LoaderState ->
  IO Linkables
newLinkables logger stateVar hsc_env pls = do
  pure Linkables {
    linkablesResolve = linkablesResolve logger stateVar hsc_env (initLinkDepsOpts hsc_env) pls,
    linkablesSelect = selectLinkDeps (initLinkDepsOpts hsc_env) (hscInterp hsc_env)
  }

#endif

installLinkables :: Logger -> MVar WorkerState -> HscEnv -> HscEnv

#if defined(LINKABLES)

installLinkables logger stateVar hsc_env =
  hsc_env {hsc_linkables = newLinkables logger stateVar}

#else

installLinkables _ _ = id

#endif
