{-# language OverloadedStrings #-}

module Internal.Debug where

import Control.Concurrent (MVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
import Data.List (stripPrefix)
import qualified Data.Map.Strict as Map
import GHC (DynFlags (..), Ghc, ModLocation (..), getSession, mi_module)
import GHC.Driver.Env (HscEnv (..))
import GHC.Types.Unique.DFM (udfmToList)
import GHC.Unit (UnitState (..), homeUnitId, installedModuleEnvElts, moduleEnvToList)
import GHC.Unit.Env (HomeUnitEnv (..), HomeUnitGraph, UnitEnv (..), UnitEnvGraph (..))
import GHC.Unit.External (ExternalPackageState (..), eucEPS)
import GHC.Unit.Finder (InstalledFindResult (InstalledFound))
import GHC.Unit.Home.ModInfo (HomePackageTable, hm_iface)
import GHC.Unit.Module.Graph (ModuleGraph, mgTransDeps)
import GHC.Utils.Outputable (SDoc, hang, hcat, ppr, text, vcat, (<+>))
import Internal.Cache (Cache (..), finderEnv)
import Internal.Log (dbgp)

entryD :: (SDoc, SDoc) -> SDoc
entryD (k, v) = hang (hcat [k, ":"]) 2 v

entry :: (String, SDoc) -> SDoc
entry (k, v) = entryD (text k, v)

entries :: [(String, SDoc)] -> SDoc
entries = vcat . fmap entry

showModGraph :: ModuleGraph -> SDoc
showModGraph g =
  vcat [ppr from <+> text "->" <+> hcat (ppr <$> toList to) | (from, to) <- Map.toList (mgTransDeps g)]

showFinderCache :: MVar Cache -> FilePath -> IO SDoc
showFinderCache var tmp = do
  Cache {finder} <- readMVar var
  modules <- finderEnv finder
  pure $ vcat [hcat [ppr m, ":"] <+> showLoc r | (m, r) <- installedModuleEnvElts modules]
  where
    showLoc = \case
      InstalledFound ModLocation {ml_hs_file} _
        | Just path <- ml_hs_file
        , Just rel <- stripPrefix tmp path
        -> text (dropWhile ('/' ==) rel)
        | otherwise
        -> "external"
      _ -> "no loc"

showEps :: ExternalPackageState -> IO SDoc
showEps EPS {..} = do
  pure $ entries $ [
    ] ++ if False then [pit] else []
  where
    pit = ("pit", vcat [ppr m <+> ppr (mi_module iface) | (m, iface) <- moduleEnvToList eps_PIT])

showUnitState :: UnitState -> SDoc
showUnitState UnitState {..} =
  entries [
    ("preloadClosure", ppr preloadClosure)
  ]

showHomeUnitDflags :: DynFlags -> SDoc
showHomeUnitDflags DynFlags {..} =
  entries [
    ("homeUnitId", ppr homeUnitId_)
  ]

showHpt :: HomePackageTable -> SDoc
showHpt hpt =
  vcat [entryD (ppr k, ppr (mi_module (hm_iface hmi))) | (k, hmi) <- udfmToList hpt]

showHomeUnitEnv :: HomeUnitEnv -> SDoc
showHomeUnitEnv HomeUnitEnv {..} =
  entries [
    ("units", showUnitState homeUnitEnv_units),
    ("dflags", showHomeUnitDflags homeUnitEnv_dflags),
    ("hpt", showHpt homeUnitEnv_hpt),
    ("home_unit", ppr (homeUnitId <$> homeUnitEnv_home_unit))
  ]

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

showEnv :: MVar Cache -> FilePath -> Ghc ()
showEnv cache tmp = do
  HscEnv {..} <- getSession
  unit_env <- liftIO (showUnitEnv hsc_unit_env)
  finder <- liftIO (showFinderCache cache tmp)
  dbgp $ entries [
    ("targets", ppr hsc_targets),
    ("mod_graph", showModGraph hsc_mod_graph),
    ("finder", finder),
    ("unit_env", unit_env)
    ]
