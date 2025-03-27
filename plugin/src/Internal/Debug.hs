{-# language OverloadedStrings, CPP #-}

module Internal.Debug where

import qualified Data.Map.Strict as Map
import GHC (DynFlags (..), mi_module)
import GHC.Types.Unique.DFM (udfmToList)
import GHC.Types.Unique.Map (nonDetEltsUniqMap)
import GHC.Unit (UnitDatabase (..), UnitId, UnitState (..), homeUnitId, moduleEnvToList, unitPackageId)
import GHC.Unit.Env (HomeUnitEnv (..), HomeUnitGraph, UnitEnv (..), UnitEnvGraph (..))
import GHC.Unit.External (ExternalPackageState (..), eucEPS)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomePackageTable, hm_iface)
import GHC.Unit.Module.Graph (ModuleGraph)
import GHC.Utils.Outputable (Outputable, SDoc, comma, hang, hcat, ppr, punctuate, text, vcat, (<+>))
import System.FilePath (takeDirectory, takeFileName)

#if __GLASGOW_HASKELL__ < 911 && !defined(MWB)

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

#if __GLASGOW_HASKELL__ < 911 && !defined(MWB)

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
--
-- showFindResult :: FindResult -> SDoc
-- showFindResult = \case
--   Found m _ -> "Found" <+> maybe "none" text (ml_hs_file m)
--   NoPackage u -> "NoPackage" <+> ppr u
--   FoundMultiple ms -> "FoundMultiple" <+> ppr ms
--   NotFound {..} -> "NotFound" <+> ppr fr_pkg <+> ppr fr_mods_hidden <+> ppr fr_pkgs_hidden <+> ppr (uuUnit <$> fr_unusables)
--
-- showInstalledFindResult :: InstalledFindResult -> SDoc
-- showInstalledFindResult = \case
--   InstalledFound m _ -> "Found" <+> maybe "none" text (ml_hs_file m)
--   InstalledNoPackage u -> "NoPackage" <+> ppr u
--   InstalledNotFound _ u -> "NotFound" <+> ppr u
