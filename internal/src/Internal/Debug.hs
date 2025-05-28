{-# language OverloadedStrings, CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Internal.Debug where

import Control.DeepSeq (deepseq, rnf)
import qualified Data.Map.Strict as Map
import GHC (DynFlags (..), ModLocation (..), ModSummary (..), mi_module)
import GHC.Linker.Types (Linkable (..), Unlinked (..))
import GHC.Types.Name.Env (seqEltsNameEnv)
import GHC.Types.TyThing (pprShortTyThing)
import GHC.Types.Unique.DFM (udfmToList)
import GHC.Types.Unique.Map (nonDetEltsUniqMap)
import GHC.Unit (UnitDatabase (..), UnitId (..), UnitState (..), homeUnitId, moduleEnvToList, unitPackageId)
import GHC.Unit.Env (HomeUnitEnv (..), HomeUnitGraph, UnitEnv (..), UnitEnvGraph (..), unitEnv_elts)
import GHC.Unit.External (ExternalPackageState (..), eucEPS)
import GHC.Unit.Finder (InstalledFindResult (..))
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomeModLinkable (..), HomePackageTable, eltsHpt, hm_iface)
import GHC.Unit.Module.Graph (ModuleGraph)
import GHC.Unit.Module.ModDetails (ModDetails (..))
import GHC.Utils.Outputable (Outputable, SDoc, comma, hang, hcat, ppr, punctuate, showPprUnsafe, text, vcat, (<+>))
import System.FilePath (takeDirectory, takeFileName)

#if !MIN_VERSION_GLASGOW_HASKELL(9,11,0,0) && !defined(MWB)

import Data.Foldable (toList)
import GHC.Unit.Module.Graph (mgTransDeps)

#else

import GHC.Unit.Module.Graph (mgModSummaries')

#endif

entryD :: (SDoc, SDoc) -> SDoc
entryD (k, v) = hang (hcat [k, ":"]) 2 v

entry :: (String, SDoc) -> SDoc
entry (k, v) = entryD (text k, v)

entries :: [(String, SDoc)] -> SDoc
entries = vcat . fmap entry

showMap ::
  Outputable a =>
  (b -> SDoc) ->
  [(a, b)] ->
  SDoc
showMap pprB m =
  vcat [ppr from <+> text "->" <+> (pprB to) | (from, to) <- m]

#if !MIN_VERSION_GLASGOW_HASKELL(9,11,0,0) && !defined(MWB)

showModGraph :: ModuleGraph -> SDoc
showModGraph g =
  showMap (ppr . toList) (Map.toList (mgTransDeps g))

#else

showModGraph :: ModuleGraph -> SDoc
showModGraph g =
  ppr (mgModSummaries' g)

#endif

showEps :: ExternalPackageState -> IO SDoc
showEps EPS {..} = do
  pure $ entries $ [
    ] ++ if False then [pit] else []
  where
    pit = ("pit", vcat [ppr m <+> ppr (mi_module iface) | (m, iface) <- moduleEnvToList eps_PIT])

showUnitState :: UnitState -> SDoc
showUnitState UnitState {..} =
  entries $ [
    ("homeUnitDepends", ppr homeUnitDepends)
  ] ++
  if False
  then [("unitInfoMap", ppr (ppr . unitPackageId <$> nonDetEltsUniqMap unitInfoMap))]
  else []

showHomeUnitDflags :: DynFlags -> SDoc
showHomeUnitDflags DynFlags {..} =
  entries [
    ("homeUnitId", ppr homeUnitId_)
  ]

showHpt :: HomePackageTable -> SDoc
showHpt hpt =
  hcat (punctuate comma [ppr (mi_module hm_iface) | (_, HomeModInfo {..}) <- udfmToList hpt])
   -- <+> ppr hm_linkable

showDbPath :: UnitDatabase UnitId -> SDoc
showDbPath UnitDatabase {unitDatabasePath} =
  text (takeFileName (takeDirectory unitDatabasePath))

showHomeUnitEnvShort :: HomeUnitEnv -> SDoc
showHomeUnitEnvShort HomeUnitEnv {..} =
  entries [
    ("deps", ppr homeUnitEnv_units.homeUnitDepends),
    ("dbs", maybe (text "not loaded") (ppr . fmap showDbPath) homeUnitEnv_unit_dbs),
    ("hpt", showHpt homeUnitEnv_hpt)
  ]

showHomeUnitEnv :: HomeUnitEnv -> SDoc
showHomeUnitEnv HomeUnitEnv {..} =
  entries [
    ("units", showUnitState homeUnitEnv_units),
    ("homeUnitEnv_unit_dbs", ppr homeUnitEnv_unit_dbs),
    ("dflags", showHomeUnitDflags homeUnitEnv_dflags),
    ("hpt", showHpt homeUnitEnv_hpt),
    ("home_unit", ppr (homeUnitId <$> homeUnitEnv_home_unit))
  ]

showHugShort :: HomeUnitGraph -> SDoc
showHugShort (UnitEnvGraph hug) =
  vcat [entryD ((ppr k), (showHomeUnitEnvShort e)) | (k, e) <- Map.toList hug]

showHug :: HomeUnitGraph -> SDoc
showHug (UnitEnvGraph hug) =
  vcat [entryD ((ppr k), (showHomeUnitEnv e)) | (k, e) <- Map.toList hug]

showUnitEnv :: UnitEnv -> IO SDoc
showUnitEnv UnitEnv {..} = do
  eps <- showEps =<< eucEPS ue_eps
  pure $ entries [
    ("eps", eps),
    ("hug", showHug ue_home_unit_graph),
    ("current_unit", ppr ue_current_unit)
    ]

forceLocation :: ModLocation -> ()
forceLocation ModLocation {..} =
  rnf (
    ml_hs_file,
    ml_hi_file,
    ml_dyn_hi_file,
    ml_obj_file,
    ml_dyn_obj_file,
    ml_hie_file
  )

forceUnitId :: UnitId -> ()
forceUnitId (UnitId s) = rnf s

forceInstalledFindResult :: InstalledFindResult -> ()
forceInstalledFindResult = \case
  InstalledFound loc m -> rnf (forceLocation loc, m)
  InstalledNoPackage uid -> rnf (forceUnitId uid)
  InstalledNotFound path uid -> rnf (path, (forceUnitId <$> uid))

pprInstalledFindResult :: InstalledFindResult -> SDoc
pprInstalledFindResult = \case
  InstalledFound _ m -> ppr m
  InstalledNoPackage _ -> text "no package"
  InstalledNotFound _ _ -> text "not found"

instance Outputable InstalledFindResult where
  ppr = pprInstalledFindResult

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

forceHug :: HomeUnitGraph -> HomeUnitGraph
forceHug hug =
  seq (sum (forceHue <$> unitEnv_elts hug)) hug
  where
    forceHue (_, HomeUnitEnv {..}) =
      sum (hmi <$> eltsHpt homeUnitEnv_hpt)

    hmi i =
      seq (forceHmi i) (1 :: Int)
