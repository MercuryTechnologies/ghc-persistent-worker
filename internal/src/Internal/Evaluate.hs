module Internal.Evaluate
  ( evaluate,
  ) where

import Control.Concurrent (modifyMVar, withMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Maybe (mapMaybe)
import GHC (
  Ghc,
  getModuleGraph,
  getSession,
  getSessionDynFlags,
  isLoaded,
  mgModSummaries,
  runTcInteractive,
  setInteractiveDynFlags,
  setSession,
  )
import GHC.Data.Bag (emptyBag)
import GHC.Driver.DynFlags (DynFlags(packageFlags), GeneralFlag (Opt_UseBytecodeRatherThanObjects), gopt_set)
import GHC.Driver.Env (hsc_HPT, hsc_home_unit, hscInterp, runInteractiveHsc, hscSetActiveUnitId)
import GHC.Driver.Env.Types (HscEnv (hsc_IC), hsc_mod_graph, hsc_targets)
import GHC.Driver.Errors.Types (hoistTcRnMessage)
import GHC.Driver.Main (hscParseStmtWithLocation, ioMsgMaybe)
import GHC.Driver.Monad (modifySession)
import GHC.Iface.Load (loadSrcInterface)
import GHC.Rename.Names (importsFromIface)
import GHC.Runtime.Context (
  InteractiveContext (..),
  InteractiveImport (..),
  replaceImportEnv,
  )
import GHC.Runtime.Eval (execLineNumber, execOptions, execSourceFile, execStmt, setContext)
import GHC.Runtime.Eval.Types (
  ExecResult (..),
  IcGlobalRdrEnv (..),
  )
import GHC.Tc.Utils.Env (lookupGlobal)
import GHC.Types.Avail (AvailInfo (..))
import GHC.Types.Name (nameOccName, pprName)
import GHC.Types.Name.Reader (
  GlobalRdrEltX (..),
  GlobalRdrEnvX,
  GREInfo,
  IfGlobalRdrEnv,
  ImpDeclSpec (..),
  Parent (NoParent),
  hydrateGlobalRdrEnv,
  plusGlobalRdrEnv,
  )
import GHC.Types.Name.Occurrence (OccName, mkOccEnv)
import GHC.Types.PkgQual (PkgQual (NoPkgQual, ThisPkg))
import GHC.Types.Target (Target (..), TargetId (..))
import GHC.Types.TyThing (tyThingGREInfo)
import GHC.Unit (moduleUnitId)
import GHC.Unit.Finder qualified as Finder
import GHC.Unit.Finder.Types (FindResult (..))
import GHC.Unit.Home (homeUnitId)
import GHC.Unit.Home.Graph (
  HomeUnitEnv (..),
  allUnits,
  lookupHug,
  lookupHugUnit,
  pprHomeUnitGraph,
  pprHomeUnitEnv,
  unitEnv_adjust,
  )
import GHC.Unit.Home.PackageTable (hptCollectModules, pprHPT)
import GHC.Unit.Module.Graph (ModuleGraph (..))
import GHC.Unit.Module.ModIface (mi_exports)
import GHC.Unit.Types (
  Definite (..),
  GenUnit (..),
  IsBootInterface (..),
  moduleName,
  moduleUnit,
  )
import GHC.Utils.Outputable (empty, ppr, text, (<+>))
import Internal.Cache.Hpt (loadHomeUnit)
import Internal.Log (logDebugD, logTimed)
import Language.Haskell.Syntax.Module.Name (ModuleName (..), mkModuleName)
import Types.Env (Env (..))
import Types.Log (Logger (..))
import Types.State (WorkerState (..))
import Types.State.Make (MakeState (..))
import Types.Target (ModuleTarget (..))

import System.IO (hPutStrLn, stderr)

evaluate :: Env -> Maybe String -> ModuleTarget -> [String] -> String -> Ghc ()
evaluate env mHomeUnit target@(ModuleTarget modu) imports expr = do
  logTimed env.log "evaluate is called" do
    hsc_env0 <- GHC.getSession
    dflags0 <- GHC.getSessionDynFlags

    case mHomeUnit of
      Nothing -> logDebugD env.log (text "Nothing")
      Just homeUnit -> do
        logDebugD env.log (text (show homeUnit))
        hsc_env1 <- liftIO $ loadHomeUnit env.log env.state dflags0 (moduleUnitId target.mod) hsc_env0 homeUnit
        hsc_env2 <- liftIO $ withMVar env.state \ state -> pure hsc_env1 {hsc_mod_graph = state.make.moduleGraph}
        let hsc_env = hscSetActiveUnitId (moduleUnitId target.mod) (hsc_env2)
        GHC.setSession hsc_env
        dflags <- GHC.getSessionDynFlags
        GHC.setInteractiveDynFlags dflags
        let home_unit = hsc_home_unit hsc_env
            home_unit_id = homeUnitId home_unit
            uid = moduleUnitId target.mod


        let modname = moduleName modu
            pkgqual = ThisPkg home_unit_id

        result <- liftIO do
          Finder.findImportedModule hsc_env modname pkgqual

        case result of
          Found modLoc modu -> do
            let unit = moduleUnit modu
            case unit of
              RealUnit (Definite uid') ->
                logDebugD env.log (text "RealUnit" <+> ppr uid')
              VirtUnit {} -> logDebugD env.log (text "VirtUnit")
              HoleUnit -> logDebugD env.log (text "HoleUnit")
            setContext [IIModule modname]

            for_ imports $ \imp -> do
              e <- loadImport env (mkModuleName imp)
              case e of
                Left _ -> pure ()
                Right rdr_env -> updateGlobalRdrEnv env rdr_env

            r <- execStmt expr execOptions
            case r of
              ExecComplete {execResult} -> do
                case execResult of
                  Left e -> logDebugD env.log (text "complete: left" <+> text (show e))
                  Right xs -> logDebugD env.log (text "complete: right:" <+> (foldr (<+>) empty (map pprName xs)))
              ExecBreak {} -> logDebugD env.log (text "break")
            pure ()
          NoPackage _ -> logDebugD env.log (text "No Package")
          FoundMultiple _ -> logDebugD env.log (text "Found Multiple")
          NotFound {} -> logDebugD env.log (text "Not Found")

loadImport :: Env -> ModuleName -> Ghc (Either String (GlobalRdrEnvX GREInfo))
loadImport env modname = do
  hsc_env <- getSession
  logDebugD env.log ("try to import" <+> ppr modname)
  result <- liftIO $ Finder.findImportedModule hsc_env modname NoPkgQual
  case result of
    Found modLoc modu -> do
      -- logDebugD env.log (text "found" <+> ppr modu)
      -- setContext [IIModule modname]
      all_env <-
            liftIO
          $ runInteractiveHsc hsc_env
          $ ioMsgMaybe $ hoistTcRnMessage $ GHC.runTcInteractive hsc_env
          $ do
            iface <- loadSrcInterface (text "imported by GHCi") (modname) NotBoot NoPkgQual
            let es :: [AvailInfo]
                es = mi_exports iface

                convert (Avail n) = Just (nameOccName n, [GRE {gre_name = n, gre_par = NoParent, gre_lcl = True, gre_imp = emptyBag, gre_info = ()}])
                convert (AvailTC _ _) = Nothing

                converted :: [(OccName, [GlobalRdrEltX ()])]
                converted = mapMaybe convert es
                exports :: IfGlobalRdrEnv
                exports = mkOccEnv converted

                get_GRE_info nm = tyThingGREInfo <$> lookupGlobal hsc_env nm
                exports_env = hydrateGlobalRdrEnv get_GRE_info exports
            pure exports_env
      pure (Right all_env)
    _ -> do
      logDebugD env.log (text "not found or error")
      pure (Left "error")

updateGlobalRdrEnv :: Env -> GlobalRdrEnvX GREInfo -> Ghc ()
updateGlobalRdrEnv env rdr_env = do
  hsc_env <- getSession
  let old_ic         = hsc_IC hsc_env
      -- this is a redefinition of replaceImportEnv, not overwriting previous context
      extendImportEnv igre import_env = igre { igre_env = new_env }
        where
          new_env = import_env `plusGlobalRdrEnv` igre_env igre
      !final_gre_cache =
        -- ic_gre_cache old_ic `replaceImportEnv` rdr_env
        ic_gre_cache old_ic `extendImportEnv` rdr_env
  setSession
    hsc_env{ hsc_IC = old_ic {ic_gre_cache = final_gre_cache}}

checkGlobalRdrEnv :: Env -> Ghc ()
checkGlobalRdrEnv env = do
  hsc_env <- getSession
  let rdr_env = igre_env (ic_gre_cache (hsc_IC hsc_env))
  logDebugD env.log (text "==== checkGlobalRdrEnv ====")
  logDebugD env.log (ppr rdr_env)
