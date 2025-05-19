{-# LANGUAGE ViewPatterns, CPP, OverloadedStrings #-}

module Internal.CompileHpt where

import Control.DeepSeq (deepseq, rnf)
import Control.Monad (when)
import GHC (DynFlags (..), GeneralFlag (..), Ghc, GhcMonad (..), Logger, ModLocation (..), ModSummary (..), gopt)
import GHC.Driver.Env (HscEnv (..), hscUpdateHUG)
import GHC.Driver.Errors.Types (GhcMessage (..))
import GHC.Driver.Make (summariseFile)
import GHC.Driver.Monad (modifySession)
import GHC.Driver.Pipeline (compileOne)
import GHC.Linker.Types (Linkable (..), Unlinked (..))
import GHC.Runtime.Loader (initializeSessionPlugins)
import GHC.Types.Name.Env (seqEltsNameEnv)
import GHC.Types.TyThing (pprShortTyThing)
import GHC.Unit.Env (addHomeModInfoToHug, ue_unsafeHomeUnit)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomeModLinkable (..))
import GHC.Unit.Module.ModDetails (ModDetails (..))
import GHC.Utils.Monad (MonadIO (..))
import GHC.Utils.Outputable (showPprUnsafe)
import GHC.Utils.TmpFs (TmpFs, cleanCurrentModuleTempFiles, keepCurrentModuleTempFiles)
import Internal.Cache (ModuleArtifacts (..), Target (..), forceLocation)
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

cleanCurrentModuleTempFilesMaybe :: MonadIO m => Logger -> TmpFs -> DynFlags -> m ()
cleanCurrentModuleTempFilesMaybe logger tmpfs dflags =
  if gopt Opt_KeepTmpFiles dflags
    then liftIO $ keepCurrentModuleTempFiles logger tmpfs
    else liftIO $ cleanCurrentModuleTempFiles logger tmpfs

forceSummary :: ModSummary -> ()
forceSummary ModSummary {ms_location} =
  forceLocation ms_location `seq` ()

forceBytecode :: Linkable -> ()
forceBytecode LM {linkableUnlinked} =
  rnf (sum (forceUnlinked <$> linkableUnlinked))
  where
    forceUnlinked = \case
      BCOs cbc _ -> seq cbc (1 :: Int)
      _ -> 1

forceHmi :: HomeModInfo -> ()
forceHmi HomeModInfo {hm_details, hm_linkable} =
  seq (maybe () forceBytecode hm_linkable.homeMod_bytecode) $
  seq (seqEltsNameEnv (\ a -> deepseq (showPprUnsafe (pprShortTyThing a)) ()) hm_details.md_types) ()

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
  Target ->
  Ghc (Maybe ModuleArtifacts)
compileModuleWithDepsInHpt (Target src) = do
  initializeSessionPlugins
  hsc_env <- getSession
  hmi@HomeModInfo {hm_iface = iface, hm_linkable} <- liftIO do
    summResult <- summariseFile hsc_env (ue_unsafeHomeUnit (hsc_unit_env hsc_env)) mempty src Nothing Nothing
    summary <- setHiLocation hsc_env <$> eitherMessages GhcDriverMessage summResult
    result <- forceSummary summary `seq` compileOne hsc_env summary 1 1 Nothing (HomeModLinkable Nothing Nothing)
    when True do
      cleanCurrentModuleTempFilesMaybe (hsc_logger hsc_env) (hsc_tmpfs hsc_env) summary.ms_hspp_opts
    pure result
  -- !() <- liftIO $ evaluate (forceHmi hmi)
  modifySession (addDepsToHscEnv [hmi])
  -- TODO does this reduce retainers? or create extra work?
  -- modifySession discardIC
  -- TODO ???
  -- addToFinderCache
  pure (Just ModuleArtifacts {iface, bytecode = homeMod_bytecode hm_linkable})
