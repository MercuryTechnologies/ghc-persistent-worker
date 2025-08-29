{-# LANGUAGE CPP #-}

module Internal.Cache.Hpt where

import Control.Concurrent.MVar (MVar)
import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import GHC (Ghc, GhcException (..), ModIface, ModuleName, mkModule)
import GHC.Data.Maybe (MaybeErr (..))
import GHC.Driver.Env (HscEnv (..), hscActiveUnitId, hscUpdateHPT, hsc_HPT)
import GHC.Driver.Main (initModDetails)
import GHC.Iface.Errors.Ppr (readInterfaceErrorDiagnostic)
import GHC.Iface.Load (readIface)
import GHC.Linker.Types (Linkable)
import GHC.Types.Unique.DFM (addToUDFM, elemUDFM)
import GHC.Unit (Definite (..), GenUnit (..))
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomeModLinkable (..), HomePackageTable)
import GHC.Unit.Module.ModDetails (ModDetails (..))
import GHC.Utils.Outputable (ppr, ($+$))
import GHC.Utils.Panic (throwGhcExceptionIO)
import Internal.Log (Log (..), logDebug)
import Prelude hiding (log)
import Types.CachedDeps (CachedDeps (..), DepName (..))

-- | Load bytecode from an interface.
-- Used only for modules missing from the current target's HPT when restoring the Buck cache after restarting a build.
--
-- The missing fields in @ModLocation@ aren't vital for the bytecode's purpose, but it wouldn't hurt to add them
-- eventually.
-- For example, the source file is used to add debug info and find foreign export stubs.
loadCachedByteCode :: HscEnv -> FilePath -> ModIface -> ModDetails -> IO (Maybe Linkable)
#if defined(MWB_2025_07)
loadCachedByteCode hsc_env ifaceFile iface details =
   sequence (loadIfaceByteCode hsc_env iface location (md_types details))
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
#else
loadCachedByteCode _ _ _ _ =
  pure Nothing
#endif

-- | If the given module name is missing from the HPT, load the given interface from disk and store it in the module's
-- 'HomeModInfo'.
--
-- This only happens when the module is dependend upon downstream for the first time after restarting the worker with a
-- partial build.
--
-- Maybe this could reuse some stuff in @hscRecompStatus@?
loadCachedDep ::
  MVar Log ->
  HscEnv ->
  HomePackageTable ->
  ModuleName ->
  FilePath ->
  IO HomePackageTable
loadCachedDep log hsc_env hpt name ifaceFile =
  if elemUDFM name hpt
  then pure hpt
  else loadHmi
  where
    loadHmi = do
      logDebug log ("Loading HPT module from cache: " ++ ifaceFile)
      hm_iface <- loadIface
      hm_details <- initModDetails hsc_env hm_iface
      homeMod_bytecode <- loadCachedByteCode hsc_env ifaceFile hm_iface hm_details
      pure $ addToUDFM hpt name HomeModInfo {
        hm_iface,
        hm_linkable = HomeModLinkable {homeMod_object = Nothing, homeMod_bytecode},
        hm_details
      }

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

-- | Load all dependencies of the current modules from the Buck cache into the HPT if they don't exist.
--
-- When the make worker is killed by Buck at the end of a build, and the user subsequently changes some code and start a
-- new build, the state (the current HPT) is initially empty, since Buck immediately tries to compile the changed
-- module, assuming its deps to be available to the compiler.
-- A JSON file provides 'CachedDeps' to the worker, containing all interface paths for the current home unit, which we
-- restore into the HPT here.
loadCachedDeps ::
  MVar Log ->
  CachedDeps ->
  HscEnv ->
  Ghc HscEnv
loadCachedDeps log CachedDeps {local} hsc_env = do
  newHpt <- foldM loadDep (hsc_HPT hsc_env) (Map.toList local)
  pure (hscUpdateHPT (const newHpt) hsc_env)
  where
    loadDep hpt (DepName name, iface :| _) = liftIO (loadCachedDep log hsc_env hpt name iface)
