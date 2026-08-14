{-# LANGUAGE CPP #-}

module DepFilesTest where

import Control.Monad.IO.Class (liftIO)
import GHC.Data.FastString (fsLit)
import GHC.Driver.DynFlags (DynFlags (..))
import GHC.Driver.Env (HscEnv (..), hscUpdateHUG)
import GHC.Driver.Main (initHscEnv)
import GHC.Fingerprint (fingerprint0)
import GHC.Paths (libdir)
import GHC.Unit.Home.Graph (HomeUnitEnv (..), unitEnv_adjust)
import GHC.Unit.Module (mkModule, mkModuleName, stringToUnitId)
import GHC.Unit.Module.Deps (Usage (..))
#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
import GHC.Iface.Recomp.Types (IfaceSelfRecomp (..))
import GHC.Unit.Module.ModIface (ModIface, emptyFullModIface, set_mi_self_recomp)
import GHC.Utils.Binary (FingerprintWithValue (..))
#else
import GHC.Unit.Module.ModIface (ModIface, emptyFullModIface, set_mi_usages)
#endif
import GHC.Unit.Types (Definite (..), GenUnit (..), UnitId)
import GhcWorker.CompileResult (usedDepFiles)
import Hedgehog (TestT, (===))
import Test.Run (unitTest)
import Test.Tasty (TestTree, testGroup)

testEnv :: IO HscEnv
testEnv = do
  hsc_env <- initHscEnv (Just libdir)
  let
    uid = homeUnitId_ hsc_env.hsc_dflags
    setOutDir d = d {hiDir = Just "out", hiSuf_ = "dyn_hi"}
    adjust h = h {homeUnitEnv_dflags = setOutDir h.homeUnitEnv_dflags}
  pure (hscUpdateHUG (unitEnv_adjust adjust uid) hsc_env)

homeModule :: UnitId -> String -> ModIface
homeModule uid name = emptyFullModIface (mkModule (RealUnit (Definite uid)) (mkModuleName name))

setUsages :: [Usage] -> ModIface -> ModIface
#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
setUsages usages =
  set_mi_self_recomp (Just IfaceSelfRecomp {
    mi_sr_src_hash = fingerprint0,
    mi_sr_usages = usages,
    mi_sr_flag_hash = FingerprintWithValue fingerprint0 Nothing,
    mi_sr_opt_hash = fingerprint0,
    mi_sr_hpc_hash = fingerprint0,
    mi_sr_plugin_hash = fingerprint0
  })
#else
setUsages = set_mi_usages
#endif

test_usedDepFiles :: TestT IO ()
test_usedDepFiles = do
  hsc_env <- liftIO testEnv
  let
    uid = homeUnitId_ hsc_env.hsc_dflags
    files usages =
      usedDepFiles hsc_env (setUsages usages (homeModule uid "This"))
    dep = mkModuleName "Dep"

  ["out/Dep.dyn_hi.hash"] ===
    files [UsageHomeModule {
      usg_mod_name = dep,
      usg_unit_id = uid,
      usg_mod_hash = fingerprint0,
      usg_entities = [],
      usg_exports = Nothing,
      usg_safe = False
    }]

  ["out/Dep.dyn_hi"] ===
    files [UsageHomeModuleInterface {
      usg_mod_name = dep,
      usg_unit_id = uid,
      usg_iface_hash = fingerprint0
    }]

  -- We do not expect an entry for an external package module. The -hidir flag is needed to derive the path anyways.
  [] ===
    files [UsagePackageModule {
      usg_mod = mkModule (RealUnit (Definite (stringToUnitId "not-a-home-unit"))) dep,
      usg_mod_hash = fingerprint0,
      usg_safe = False
    }]

  -- addDependentFile is dropped and tracked separately in the buck2-haskell rules.
  [] ===
    files [UsageFile {
      usg_file_path = fsLit "data.json",
      usg_file_hash = fingerprint0,
      usg_file_label = Nothing
    }]

test_depFiles :: TestTree
test_depFiles =
  testGroup "dep files" [
    unitTest "usages resolve to the files GHC compares" test_usedDepFiles
  ]
