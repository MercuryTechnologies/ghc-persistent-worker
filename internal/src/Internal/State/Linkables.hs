{-# LANGUAGE CPP #-}

module Internal.State.Linkables where

import Control.Concurrent (MVar)
import GHC.Driver.Env.Types (HscEnv)
import Types.Log (Logger (..))
import Types.State (WorkerState)

#if defined(LINKABLES)

import Control.Concurrent (modifyMVar, modifyMVar_)
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
import Internal.Cache.Bytecode (touchBcoCache)
import Internal.Cache.Hpt (loadCachedByteCodeFrom)
import Internal.Error (workerErrorIO)
import Internal.State (modifyMakeState)
import Language.Haskell.Syntax.ImpExp (IsBootInterface (..))
import System.Directory (findFile)
import Types.State (WorkerState (..))
import Types.State.Make (LibLoadState (..), MakeState (..))


-- | An absent 'ModLocation' should be fatal even if we don't end up loading bytecode, since we always add it to the
-- Finder when restoring from cache.
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

-- | Load bytecode from an interface given an HPT entry.
--
-- Query the Finder for a 'ModLocation', since we don't have a file path available, which is normally used when loading
-- bytecode from cache upfront.
-- Then, compile bytecode from Core as usual.
-- If the interface has no Core, skip and return 'Nothing'.
-- Otherwise, add the bytecode 'Linkable' to the HPT in the state and return it.
-- HPTs are stored in 'IORef's, so the bytecode will be available to other compile tasks immediately.
--
-- TODO possible race: when two splices are linked concurrently, and both enter @addLazyByteCode@ with @Nothing@, we'll
-- probably compile bytecode twice.
-- Maybe just look it up in the HUG inside of @modifyMVar@ once more.
lazyLoadByteCode ::
  Logger ->
  MVar WorkerState ->
  HscEnv ->
  HomeModInfo ->
  IO (Maybe Linkable)
lazyLoadByteCode logger stateVar hsc_env hmi = do
  logger.debugD ("Loading lazy bytecode for " <+> ppr module_)
  modifyMVar stateVar \ state -> do
    location <- findLocation state.make.hug
    loadCachedByteCodeFrom hsc_env location (hm_iface hmi) (hm_details hmi) >>= \case
      Just bytecode -> insertBytecode state bytecode
      Nothing -> pure (state, Nothing)
  where
    findLocation hug = do
      result <- withFinder hsc_env hug findExactModule (hsc_units hsc_env) (Just homeUnit) (toUnitId <$> module_) NotBoot
      requireLocation hsc_env module_ result

    insertBytecode state bytecode = do
      let hm_iface = hmi.hm_iface
          new = hmi {hm_iface, hm_linkable = hmi.hm_linkable {homeMod_bytecode = Just bytecode}}
          unit = moduleUnitId (mi_module hm_iface)
      traverse_ (insertIntoHpt new) (unitEnv_lookup_maybe unit state.make.hug)
      pure (state, Just bytecode)

    insertIntoHpt new hue = addHomeModInfoToHpt new (homeUnitEnv_hpt hue)

    homeUnit = hsc_home_unit hsc_env

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

-- | If the link target is a home module that's missing bytecode (because it was restored from cache), load it from the
-- interface into the HPT and store it in the returned value.
-- 'selectLinkDeps' will then choose the bytecode for linking.
-- If the interface does not contain Core bindings, this will not have an effect.
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

-- | Wrap the native 'selectLinkDeps' to update the BCO tracking metadata.
linkablesSelect ::
  MVar WorkerState ->
  LinkDepsOpts ->
  Interp ->
  SrcSpan ->
  ([Linkable], [LinkModule], UniqDSet UnitId, [UnitId]) ->
  IO LinkDeps
linkablesSelect stateVar opts interp srcSpan resolved = do
  deps <- selectLinkDeps opts interp srcSpan resolved
  modifyMVar_ stateVar \ state ->
    pure state {make = touchBcoCache deps.ldAllLinkables state.make}
  pure deps

newLinkables ::
  Logger ->
  MVar WorkerState ->
  HscEnv ->
  LoaderState ->
  IO Linkables
newLinkables logger stateVar hsc_env pls = do
  pure Linkables {
    linkablesResolve = linkablesResolve logger stateVar hsc_env (initLinkDepsOpts hsc_env) pls,
    linkablesSelect = linkablesSelect stateVar (initLinkDepsOpts hsc_env) (hscInterp hsc_env)
  }

#endif

installLinkables :: Logger -> MVar WorkerState -> HscEnv -> HscEnv

#if defined(LINKABLES)

installLinkables logger stateVar hsc_env =
  hsc_env {hsc_linkables = newLinkables logger stateVar}

#else

installLinkables _ _ = id

#endif
