{-# LANGUAGE CPP #-}

module GhcWorker.CompileResult where

import Data.Foldable (for_)
import Data.Int (Int32)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import GHC.Driver.Env (HscEnv (..))
import GHC.Unit.Env (UnitEnv (..))
import GHC.Unit.Home.Graph (HomeUnitEnv (..), unitEnv_lookup_maybe)
import GHC.Unit.Module (moduleUnit, toUnitId)
import qualified GHC.Unit.Module as Mod
import GHC.Unit.Module.Deps (Usage (..))
import GHC.Unit.Module.ModIface (ModIface, mi_usages)
import Internal.AbiHash (AbiHash (..))
import Internal.Cache.Hpt (canonicalInterfacePath)
import System.OsPath.Extra (fromOsPath)
import Types.BuckArgs (BuckArgs (..))

-- | Right now the 'Maybe' just corresponds to the presence of the CLI argument @--abi-out@ – errors occuring while
-- reading the iface are thrown.
data CompileResult =
  CompileResult {
    abiHash :: Maybe AbiHash,
    depFiles :: [FilePath]
  }
  deriving stock (Show)

-- | The dependency files this compile consulted, to populate Buck's dep file.
--
-- Only the tagged interface and ABI hash files named here will invalidate
-- compile actions. The usages are the set GHC compares for its internal
-- recompilation check. Specific scenarios:
--
-- * A direct import changing its ABI always triggers recompilation because direct
--   imports are always recorded as usages.
-- * Something changing in a module that is never directly imported, like a re-exported
--   type, triggers recompilation because usages are recorded against the defining module.
-- * Adding or removing typeclass instances that are not imported directly still correctly triggers
--   recompilation because the export hash depends on the orphan hash and therefore the
--   ABI changes transitively (see https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/recompilation-avoidance#orphans).
-- * Changing the implementation of a function that is transitively called in a
--   TemplateHaskell splice would trigger recompilation when using
--   -fprefer-byte-code (the only case this code supports). In this case, the
--   usages record the modules imported transitively that might influence the
--   evaluation of the splice.
--
-- Note the format and file list are tightly coupled to the buck2-haskell rules. For
-- example, addDependentFiles are not represented at all in the output because they are
-- tracked separately in those rules.
usedDepFiles :: HscEnv -> ModIface -> [FilePath]
usedDepFiles hsc_env iface =
  Set.toAscList $ Set.fromList $ mapMaybe fileForUsage usages
  where
#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
    usages = case mi_usages iface of
      Just us -> us
      Nothing -> error "usedDepFiles: interface has no self-recomp info"
#else
    usages = mi_usages iface
#endif

    fileForUsage = \case
      UsagePackageModule {usg_mod} -> abiHashFileOf usg_mod
      UsageHomeModule {usg_mod_name, usg_unit_id} -> abiHashFile usg_unit_id usg_mod_name
      -- This includes the TH case where GHC compares mi_iface_hash, not just the ABI.
      -- -fprefer-byte-code is required for correctness.
      UsageHomeModuleInterface {usg_mod_name, usg_unit_id} -> interfaceFile usg_unit_id usg_mod_name
      UsageMergedRequirement {usg_mod} -> abiHashFileOf usg_mod
      -- We assume that addDependentFiles are correctly tracked separately as srcs_deps in the buck2 rules.
      -- At the time of writing, adding additional paths causes errors when these
      -- paths are not relative or contain . and ..
      UsageFile {} -> Nothing

    abiHashFileOf m = abiHashFile (toUnitId (moduleUnit m)) (Mod.moduleName m)

    abiHashFile uid name = (++ ".hash") <$> interfaceFile uid name

    interfaceFile uid name = do
      home <- unitEnv_lookup_maybe uid hsc_env.hsc_unit_env.ue_home_unit_graph
      fromOsPath <$> canonicalInterfacePath (homeUnitEnv_dflags home) name

writeResult :: BuckArgs -> Maybe AbiHash -> [FilePath] -> IO ()
writeResult args abiHash depFiles = do
  for_ abiHash \ AbiHash {path, hash} -> writeFile path hash
  for_ args.buck2Dep \ path -> writeFile path (unlines' depFiles)
  for_ args.buck2PackageDbDep \ path -> writeFile path (unlines' args.buck2PackageDb)
  where
    unlines' [] = "\n"
    unlines' xs = unlines xs

writeCloseOutput :: BuckArgs -> IO Int32
writeCloseOutput args =
  case args.closeOutput of
    Nothing -> pure 1
    Just path -> writeFile path "\n" >> pure 0
