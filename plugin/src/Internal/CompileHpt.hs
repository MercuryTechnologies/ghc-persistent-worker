{-# LANGUAGE ViewPatterns, CPP, OverloadedStrings #-}

module Internal.CompileHpt where

import Control.Monad (when)
import Data.Foldable (fold)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import GHC (
  DynFlags (..),
  GeneralFlag (..),
  Ghc,
  GhcMonad (..),
  Logger,
  ModLocation (..),
  ModSummary (..),
  gopt,
  mkGeneralLocated,
  setUnitDynFlags,
  )
import GHC.Driver.Config.Diagnostic (initDiagOpts, initPrintConfig)
import GHC.Driver.Env (HscEnv (..), hscSetActiveUnitId, hscUpdateHUG)
import GHC.Driver.Errors (printOrThrowDiagnostics)
import GHC.Driver.Errors.Types (GhcMessage (..))
import GHC.Driver.Make (summariseFile)
import GHC.Driver.Monad (modifySession)
import GHC.Driver.Pipeline (compileOne)
import GHC.Driver.Session (parseDynamicFlagsCmdLine)
import GHC.Runtime.Loader (initializeSessionPlugins)
import GHC.Unit (UnitId, UnitState (..), stringToUnitId, unitIdString)
import GHC.Unit.Env (
  HomeUnitEnv (..),
  HomeUnitGraph,
  UnitEnv (..),
  UnitEnvGraph (..),
  addHomeModInfoToHug,
  ue_unsafeHomeUnit,
  unitEnv_lookup_maybe,
  )
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomeModLinkable (..))
import GHC.Utils.Monad (MonadIO (..))
import GHC.Utils.TmpFs (TmpFs, cleanCurrentModuleTempFiles, keepCurrentModuleTempFiles)
import Internal.Cache (ModuleArtifacts (..), Target (..))
import Internal.Error (eitherMessages)

-- | Insert a compilation result into the current unit's home package table, as it is done by upsweep.
addDepsToHscEnv :: [HomeModInfo] -> HscEnv -> HscEnv
addDepsToHscEnv deps = hscUpdateHUG (\hug -> foldr addHomeModInfoToHug hug deps)

-- | Update the location of the result of @summariseFile@ to point to the locations specified on the command line, since
-- these are placed in the source file's directory by that function.
setHiLocation :: HscEnv -> ModSummary -> ModSummary
setHiLocation HscEnv {hsc_dflags = DynFlags {outputHi = Just ml_hi_file, outputFile_ = Just ml_obj_file}} summ =
  summ {ms_location = summ.ms_location {ml_hi_file, ml_obj_file}}
setHiLocation _ summ = summ

-- | Parse command line flags, used to create unit-specific @DynFlags@.
unitFlags :: [String] -> HscEnv -> Ghc DynFlags
unitFlags args HscEnv {hsc_logger, hsc_dflags = dflags0} = do
  (dflags, _, warns) <- parseDynamicFlagsCmdLine dflags0 (map (mkGeneralLocated "no loc") args)
  liftIO $ printOrThrowDiagnostics hsc_logger (initPrintConfig dflags) (initDiagOpts dflags) (GhcDriverMessage <$> warns)
  pure dflags

-- | Turn each @-package@ argument in the given list into a @-package-id@ argument if the following package name refers
-- to a home unit, and create a set of all mentioned home unit names.
-- This is needed for GHC to recognize home unit dependencies.
adaptHp :: HomeUnitGraph -> [String] -> (Set UnitId, [String])
adaptHp (UnitEnvGraph ueg) =
  spin mempty
  where
    spin seen = \case
      "-package" : p : rest
        | Map.member (stringToUnitId p) ueg
        -> (["-package-id", p] ++) <$> spin (Set.insert (stringToUnitId p) seen) rest
      arg : rest -> (arg :) <$> spin seen rest
      [] -> (seen, [])

-- | Return the given unit's dependencies.
homeUnitDeps :: HscEnv -> UnitId -> Maybe [UnitId]
homeUnitDeps hsc_env target = do
  HomeUnitEnv {homeUnitEnv_units = UnitState {homeUnitDepends}} <- unitEnv_lookup_maybe target hug
  pure homeUnitDepends
  where
    hug = hsc_env.hsc_unit_env.ue_home_unit_graph

-- | Assemble @-package-id@ arguments for the current unit's dependencies, omitting those that are present in the
-- provided set, which are the current module's deps specified by Buck.
homeUnitDepFlags :: HscEnv -> Set UnitId -> UnitId -> [String]
homeUnitDepFlags hsc_env explicit target =
  concat [["-package-id", unitIdString u] | u <- fold prev_deps, not (Set.member u explicit)]
  where
    prev_deps = homeUnitDeps hsc_env target

-- | Update the current unit's @DynFlags@ stored in the unit env, and reinitialize its unit state.
-- Since different modules in the same unit may have arbitrary subsets of the unit's package dependencies when Buck
-- compiles them, we take the union of existing and new dependencies.
initUnit :: [String] -> Ghc ()
initUnit specific = do
  hsc_env0 <- getSession
  let current = hsc_env0.hsc_unit_env.ue_current_unit
      (explicit, withPackageId) = adaptHp hsc_env0.hsc_unit_env.ue_home_unit_graph specific
      unitOptions = withPackageId ++ homeUnitDepFlags hsc_env0 explicit current
  dflags <- unitFlags unitOptions hsc_env0
  setUnitDynFlags current dflags
  modifySession (hscSetActiveUnitId current)

-- | Not used yet.
cleanCurrentModuleTempFilesMaybe :: MonadIO m => Logger -> TmpFs -> DynFlags -> m ()
cleanCurrentModuleTempFilesMaybe logger tmpfs dflags =
  if gopt Opt_KeepTmpFiles dflags
    then liftIO $ keepCurrentModuleTempFiles logger tmpfs
    else liftIO $ cleanCurrentModuleTempFiles logger tmpfs

-- | Compile a module with multiple home units in the session state, using the home package table to look up
-- dependencies.
--
-- First, update the current unit's configuration to include this module's dependencies.
-- Buck only provides @-package@ flags for deps that are used by a given module, while the unit state is designed to be
-- initialized up front with the deps of all modules.
-- Note: This should soon be obsolete, since we now have full control over the metadata step.
--
-- Next, perform the steps that usually happen in make mode's upsweep:
-- - Create a @ModSummary@ using @summariseFile@
-- - Call the module compilation function @compileOne@
-- - Store the resulting @HomeModInfo@ in the current unit's home package table.
compileModuleWithDepsInHpt ::
  [String] ->
  Target ->
  Ghc (Maybe ModuleArtifacts)
compileModuleWithDepsInHpt _ (Target src) = do
  initializeSessionPlugins
  hsc_env <- getSession
  hmi@HomeModInfo {hm_iface = iface, hm_linkable} <- liftIO do
    summResult <- summariseFile hsc_env (ue_unsafeHomeUnit (hsc_unit_env hsc_env)) mempty src Nothing Nothing
    summary <- setHiLocation hsc_env <$> eitherMessages GhcDriverMessage summResult
    result <- compileOne hsc_env summary 1 100000 Nothing (HomeModLinkable Nothing Nothing)
    -- This deletes assembly files too early
    when False do
      cleanCurrentModuleTempFilesMaybe (hsc_logger hsc_env) (hsc_tmpfs hsc_env) summary.ms_hspp_opts
    pure result
  modifySession (addDepsToHscEnv [hmi])
  pure (Just ModuleArtifacts {iface, bytecode = homeMod_bytecode hm_linkable})
