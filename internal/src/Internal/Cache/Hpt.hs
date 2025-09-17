{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}

module Internal.Cache.Hpt where

import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty ((:|)), groupBy)
import Data.Time (getCurrentTime)
import Data.Traversable (for)
import GHC (DynFlags (..), Ghc, GhcException (..), ModIface, ModIface_ (..), ModLocation (..), ModuleName, mkModule)
import GHC.Data.Maybe (MaybeErr (..))
import GHC.Driver.DynFlags (GhcMode (..))
import GHC.Driver.Env (HscEnv (..), hscActiveUnitId, hscSetActiveUnitId, hscUpdateFlags, hscUpdateHPT, hsc_HPT)
import GHC.Driver.Main (initModDetails, initWholeCoreBindings)
import GHC.Iface.Errors.Ppr (readInterfaceErrorDiagnostic)
import GHC.Iface.Load (readIface)
import GHC.Linker.Types (Linkable (..))
import GHC.Types.Unique.DFM (addToUDFM, elemUDFM)
import GHC.Unit (Definite (..), GenUnit (..))
import GHC.Unit.Env (UnitEnv (..), unitEnv_member)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomeModLinkable (..))
import GHC.Unit.Module.ModDetails (ModDetails (..))
import GHC.Unit.Module.WholeCoreBindings (WholeCoreBindings (..))
import GHC.Utils.Misc (modificationTimeIfExists)
import GHC.Utils.Outputable (ppr, ($+$))
import GHC.Utils.Panic (throwGhcExceptionIO)
import Prelude hiding (log)
import Types.CachedDeps (
  CachedDeps (..),
  CachedInterface (..),
  CachedProjectDep (..),
  JsonFs (..),
  cachedProjectDepInterface,
  )
import Types.Log (Logger (..))

-- This preprocessor variable indicates that we're building with a GHC that has the final version of the oneshot
-- bytecode patch.
#if defined(MWB_2025_07) || MIN_VERSION_GLASGOW_HASKELL(9,12,0,0)
import GHC.Linker.Types (LinkablePart (..))
#else
import GHC.Linker.Types (Unlinked (..))
#endif

-- | Load bytecode from an interface.
-- Used only for modules missing from the current target's HPT when restoring the Buck cache after restarting a build.
--
-- The missing fields in @ModLocation@ aren't vital for the bytecode's purpose, but it wouldn't hurt to add them
-- eventually.
-- For example, the source file is used to add debug info and find foreign export stubs.
loadCachedByteCode :: HscEnv -> FilePath -> ModIface -> ModDetails -> IO (Maybe Linkable)
loadCachedByteCode hsc_env ifaceFile iface details =
  for core_bindings \ wcb -> do
    linkable <- bcoLinkable [CoreBindings wcb]
    initWholeCoreBindings hsc_env iface details linkable
   where
    location =
      ModLocation {
        ml_hs_file = Nothing,
        ml_hi_file = ifaceFile,
        ml_dyn_hi_file = ifaceFile,
        ml_obj_file = error "loadCachedByteCode",
        ml_dyn_obj_file = error "loadCachedByteCode",
        ml_hie_file = error "loadCachedByteCode"
      }

    core_bindings =
      mi_extra_decls <&> \ wcb_bindings ->
#if defined(MWB_2025_07) || MIN_VERSION_GLASGOW_HASKELL(9,12,0,0)
        WholeCoreBindings {wcb_mod_location = location, wcb_foreign = mi_foreign, ..}
#else
        WholeCoreBindings {wcb_mod_location = location, ..}
#endif
      where
        ModIface {mi_module = wcb_module, ..} = iface

    bcoLinkable parts = do
      if_time <- modificationTimeIfExists (ml_hi_file location)
      time <- maybe getCurrentTime pure if_time
#if defined(MWB_2025_07) || MIN_VERSION_GLASGOW_HASKELL(9,12,0,0)
      return $! Linkable time (mi_module iface) parts
#else
      return $! LM time (mi_module iface) parts
#endif

-- | If the given module name is missing from the HPT, load the given interface from disk and store it in the module's
-- 'HomeModInfo'.
--
-- This only happens when the module is depended upon downstream for the first time after restarting the worker with a
-- partial build.
--
-- Maybe this could reuse some stuff in @hscRecompStatus@?
loadCachedDep ::
  Logger ->
  ModuleName ->
  HscEnv ->
  FilePath ->
  IO HscEnv
loadCachedDep log name hsc_env ifaceFile =
  if elemUDFM name hpt
  then pure hsc_env
  else loadHmi
  where
    loadHmi = do
      log.debug ("Loading HPT module from cache: " ++ ifaceFile)
      hm_iface <- loadIface
      hm_details <- initModDetails hsc_env hm_iface
      homeMod_bytecode <- loadCachedByteCode hsc_env ifaceFile hm_iface hm_details
      let new = addToUDFM hpt name HomeModInfo {
        hm_iface,
        hm_linkable = HomeModLinkable {homeMod_object = Nothing, homeMod_bytecode},
        hm_details
      }
      pure (hscUpdateHPT (const new) hsc_env)

    -- @readIface@ needs the dflags only for platform/ways, so we don't need the unit dflags
    loadIface =
      ifaceResult =<< readIface (hsc_dflags hsc_env) (hsc_NC hsc_env) (toModule name) ifaceFile

    ifaceResult = \case
      Succeeded i ->
        pure i
      Failed err ->
        let msg = ppr name $+$ readInterfaceErrorDiagnostic err
        in throwGhcExceptionIO (PprProgramError "Loading cached interface failed" msg)

    toModule = mkModule (RealUnit (Definite uid))

    uid = hscActiveUnitId hsc_env

    hpt = hsc_HPT hsc_env

-- | Load all dependencies of the current module from the Buck cache into the HPT if they don't exist.
--
-- When the make worker is killed by Buck at the end of a build, and the user subsequently changes some code and starts
-- a new build, the state (the current HPT) is initially empty, since Buck immediately tries to compile the changed
-- module, assuming its deps to be available to the compiler.
-- A JSON file provides 'CachedDeps' to the worker, containing all interface paths for the current home unit, which we
-- restore into the HPT here.
loadCachedDeps ::
  Logger ->
  CachedDeps ->
  HscEnv ->
  Ghc HscEnv
loadCachedDeps log CachedDeps {home_unit, project} hsc_env0 = do
  hsc_env1 <- foldM loadDepUnit (setDyn hsc_env0) projectByUnit
  let hsc_env2 = hscSetActiveUnitId (hscActiveUnitId hsc_env0) hsc_env1
  loadActiveUnit (setDyn hsc_env2) home_unit
  where
    setDyn = hscUpdateFlags \ d -> d {ghcMode = CompManager}

    -- If the unit isn't present in the unit env, it wasn't built by a worker, since it would have been loaded in the
    -- metadata restoration step.
    loadDepUnit hsc_env mods@(CachedProjectDep {package = JsonFs uid} :| _) =
      if unitEnv_member uid hsc_env.hsc_unit_env.ue_home_unit_graph
      then loadActiveUnit (hscSetActiveUnitId uid hsc_env) (cachedProjectDepInterface <$> toList mods)
      else pure hsc_env

    loadActiveUnit = foldM loadDep

    loadDep hsc_env CachedInterface {name = JsonFs name, interfaces = iface :| _} =
      liftIO (loadCachedDep log name hsc_env iface)

    projectByUnit = groupBy (on (==) (.package)) project
