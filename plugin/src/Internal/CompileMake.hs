module Internal.CompileMake where

import Control.Monad (forM, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (Bifunctor (second))
import Data.List (partition, (\\))
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Traversable (for)
import qualified GHC
import GHC (
  DynFlags (..),
  Ghc,
  GhcException (..),
  GhcMonad (..),
  LoadHowMuch (..),
  mkGeneralLocated,
  parseTargetFiles,
  setTargets,
  unLoc,
  )
import GHC.Driver.Config.Diagnostic (initDiagOpts, initPrintConfig)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Errors (printOrThrowDiagnostics)
import GHC.Driver.Errors.Types (GhcMessage (GhcDriverMessage))
import GHC.Driver.Phases (isHaskellishTarget)
import GHC.Driver.Ppr (showSDoc)
import GHC.Driver.Session (parseDynamicFlagsCmdLine, updatePlatformConstants)
import GHC.Types.Target (TargetId (..))
import GHC.Unit (UnitId)
import GHC.Unit.Env (
  HomeUnitEnv (..),
  HomeUnitGraph,
  assertUnitEnvInvariant,
  initUnitEnv,
  mkHomeUnitEnv,
  unitEnv_keys,
  unitEnv_lookup,
  unitEnv_new,
  )
import GHC.Unit.Home.ModInfo (emptyHomePackageTable)
import qualified GHC.Unit.State as State
import GHC.Utils.Misc (ordNubOn)
import GHC.Utils.Outputable (IsDoc (vcat), IsLine (text, (<+>)), Outputable (ppr), hang)
import GHC.Utils.Panic (throwGhcException)
import Internal.Log (dbgp)
import System.FilePath (isRelative, (</>))

offsetDynFlags :: DynFlags -> DynFlags
offsetDynFlags dflags =
  dflags { hiDir = c hiDir
         , objectDir  = c objectDir
         , stubDir = c stubDir
         , hieDir  = c hieDir
         , dumpDir = c dumpDir  }

  where
    c f = augment_maybe (f dflags)

    augment_maybe Nothing = Nothing
    augment_maybe (Just f) = Just (augment f)
    augment f | isRelative f, Just offset <- workingDirectory dflags = offset </> f
              | otherwise = f

checkDuplicateUnits :: DynFlags -> [(FilePath, DynFlags)] -> Ghc ()
checkDuplicateUnits dflags flags =
  unless (null duplicate_ids)
         (throwGhcException $ CmdLineError $ showSDoc dflags multi_err)

  where
    uids = map (second homeUnitId_) flags
    deduplicated_uids = ordNubOn snd uids
    duplicate_ids = Set.fromList (map snd uids \\ map snd deduplicated_uids)

    duplicate_flags = filter (flip Set.member duplicate_ids . snd) uids

    one_err (fp, home_uid) = text "-" <+> ppr home_uid <+> text "defined in" <+> text fp

    multi_err =
      hang (text "Multiple units with the same unit-id:")
           2
           (vcat (map one_err duplicate_flags))

createUnitEnvFromFlags :: NonEmpty DynFlags -> (HomeUnitGraph, UnitId)
createUnitEnvFromFlags unitDflags =
  let
    newInternalUnitEnv dflags = mkHomeUnitEnv dflags emptyHomePackageTable Nothing
    unitEnvList = (\dflags -> (homeUnitId_ dflags, newInternalUnitEnv dflags)) <$> unitDflags
    activeUnit = fst $ NonEmpty.head unitEnvList
  in
    (unitEnv_new (Map.fromList (NonEmpty.toList (unitEnvList))), activeUnit)

step1 :: NonEmpty [String] -> Ghc ()
step1 argv = do
  hsc_env <- GHC.getSession
  let logger = hsc_logger hsc_env
      dflags0 = hsc_dflags hsc_env
  units <- for argv \ args -> do
    (dflags2, fileish_args, warns) <- parseDynamicFlagsCmdLine dflags0 (map (mkGeneralLocated "no loc") args)
    liftIO $ printOrThrowDiagnostics logger (initPrintConfig dflags2) (initDiagOpts dflags2) (GhcDriverMessage <$> warns)
    let (dflags3, srcs, _) = parseTargetFiles dflags2 (map unLoc fileish_args)
        dflags4 = offsetDynFlags dflags3
        (hs_srcs, _) = partition isHaskellishTarget srcs
    pure (dflags4, hs_srcs)

  let unitDflags = fst <$> units
  checkDuplicateUnits dflags0 (NonEmpty.toList (("no loc",) <$> unitDflags))
  let (initial_home_graph, mainUnitId) = createUnitEnvFromFlags unitDflags
      home_units = unitEnv_keys initial_home_graph
  home_unit_graph <- forM initial_home_graph $ \homeUnitEnv -> do
    let cached_unit_dbs = homeUnitEnv_unit_dbs homeUnitEnv
        hue_flags = homeUnitEnv_dflags homeUnitEnv
        dflags = homeUnitEnv_dflags homeUnitEnv
    (dbs,unit_state,home_unit,mconstants) <- liftIO $ State.initUnits logger hue_flags cached_unit_dbs home_units
    updated_dflags <- liftIO $ updatePlatformConstants dflags mconstants
    pure $ HomeUnitEnv
      { homeUnitEnv_units = unit_state
      , homeUnitEnv_unit_dbs = Just dbs
      , homeUnitEnv_dflags = updated_dflags
      , homeUnitEnv_hpt = emptyHomePackageTable
      , homeUnitEnv_home_unit = Just home_unit
      }
  let dflags = homeUnitEnv_dflags $ unitEnv_lookup mainUnitId home_unit_graph
  unitEnv <- assertUnitEnvInvariant <$> (liftIO $ initUnitEnv mainUnitId home_unit_graph (ghcNameVersion dflags) (targetPlatform dflags))
  GHC.setSession hsc_env { hsc_unit_env = unitEnv }

step2 :: [(String, UnitId)] -> Ghc ()
step2 targets = do
  setTargets [GHC.Target (TargetFile path Nothing) True unit Nothing | (path, unit) <- targets]
  res <- GHC.load LoadAllTargets
  dbgp res
