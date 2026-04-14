module Internal.Evaluate
  ( evaluate,
  ) where

import Control.Concurrent (modifyMVar, withMVar)
import Control.Monad.IO.Class (liftIO)
import GHC (
  Ghc,
  getModuleGraph,
  getSession,
  getSessionDynFlags,
  isLoaded,
  mgModSummaries,
  setInteractiveDynFlags,
  setSession,
  )
import GHC.Driver.DynFlags (DynFlags(packageFlags), GeneralFlag (Opt_UseBytecodeRatherThanObjects), gopt_set)
import GHC.Driver.Env (hsc_HPT, hsc_home_unit, hscInterp, runInteractiveHsc, hscSetActiveUnitId)
import GHC.Driver.Env.Types (hsc_IC, hsc_mod_graph, hsc_targets)
import GHC.Driver.Main (hscParseStmtWithLocation)
import GHC.Driver.Monad (modifySession)
import GHC.Runtime.Context (InteractiveImport (..))
import GHC.Runtime.Eval (execLineNumber, execOptions, execSourceFile, execStmt, setContext)
import GHC.Runtime.Eval.Types (ExecResult (..))
import GHC.Types.Name (pprName)
import GHC.Types.PkgQual (PkgQual (ThisPkg))
import GHC.Types.Target (Target (..), TargetId (..))
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
import GHC.Unit.Types (
  Definite (..),
  GenUnit (..),
  moduleName,
  moduleUnit,
  )
import GHC.Utils.Outputable (empty, ppr, text, (<+>))
import Internal.Cache.Hpt (loadHomeUnit)
import Internal.Log (logDebugD, logTimed)
import Language.Haskell.Syntax.Module.Name (ModuleName (..))
import Types.Env (Env (..))
import Types.Log (Logger (..))
import Types.State (WorkerState (..))
import Types.State.Make (MakeState (..))
import Types.Target (ModuleTarget (..))

import System.IO (hPutStrLn, stderr)

evaluate :: Env -> Maybe String -> ModuleTarget -> String -> Ghc ()
evaluate env mHomeUnit target@(ModuleTarget modu) input = do
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
            r <- execStmt input execOptions
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
