module TestMake where

import Control.Concurrent (readMVar)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (StateT, state)
import Data.Foldable (find, fold, for_)
import Data.Functor (void, (<&>))
import Data.Map.Strict (Map, (!?))
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC (
  DynFlags (..),
  Ghc,
  GhcException (..),
  GhcMode (..),
  ModSummary (..),
  ModuleGraph,
  getSession,
  guessTarget,
  mgModSummaries,
  mkModuleGraph,
  ms_mod_name,
  setTargets,
  unLoc,
  )
import GHC.Driver.Env (HscEnv (..), hscUpdateFlags, hsc_home_unit)
import GHC.Driver.Make (downsweep)
import GHC.Driver.Monad (modifySession, modifySessionM, withTempSession)
import GHC.Runtime.Loader (initializeSessionPlugins)
import GHC.Types.Error (unionManyMessages)
import GHC.Unit (UnitId, UnitState (..), homeUnitId, moduleUnitId, stringToUnitId)
import GHC.Unit.Env (HomeUnitEnv (..), UnitEnv (..), ue_findHomeUnitEnv, unitEnv_union)
import GHC.Unit.Finder (addHomeModuleToFinder)
import GHC.Unit.Module.Graph (unionMG)
import GHC.Utils.Outputable (ppr, text, (<+>))
import GHC.Utils.Panic (throwGhcExceptionIO)
import Internal.Args (Args (..))
import Internal.Cache (Cache (..), Target (..), mergeHugs, newFinderCache, updateModuleGraph)
import Internal.CompileHpt (adaptHp, compileHpt, initUnit)
import Internal.Debug (showHugShort, showModGraph)
import Internal.Log (dbg, dbgp, dbgs, newLog)
import Internal.Session (Env (..), dummyLocation, withGhcGeneral, withGhcInSession, withUnitSpecificOptions)
import Prelude hiding (log)
import System.Directory (listDirectory)
import System.FilePath (dropExtension, takeDirectory, takeExtension, takeFileName, (</>))
import TestSetup (Conf (..), UnitConf (UnitConf, db, externalDeps, uid), UnitMod (..))

-- -- TODO make a unit test out of this
-- debugFinder :: Ghc ()
-- debugFinder = do
--   hsc_env <- getSession
--   cache <- liftIO $ readIORef (fcModuleCache (hsc_FC hsc_env))
--   when True do
--     dbgp (fmap showInstalledFindResult <$> installedModuleEnvElts cache)
--   res <- liftIO $ findImportedModule hsc_env (mkModuleName "Err") NoPkgQual
--   dbgp (showFindResult res)
--
loadModuleGraph :: Env -> UnitMod -> [String] -> Ghc ModuleGraph
loadModuleGraph env UnitMod {src} specific = do
  cache <- liftIO $ readMVar env.cache
  (_, module_graph) <- withTempSession (maybe id restoreHug cache.hug . maybe id restoreModuleGraph cache.moduleGraph) do
    modifySessionM \ hsc_env -> do
      hsc_FC <- liftIO $ newFinderCache env.cache cache (Target "metadata")
      pure hsc_env {hsc_FC}
    dbg (unwords specific)
    initUnit specific
    mg <- hsc_mod_graph <$> getSession
    dbgp ("existing graph:" <+> showModGraph mg)
    dbgp ("hug:" <+> showModGraph mg)
    dbgp . showHugShort . ue_home_unit_graph . hsc_unit_env =<< getSession
    hsc_env <- getSession
    let ue = hsc_env.hsc_unit_env
        currentId = hsc_env.hsc_unit_env.ue_current_unit
        hue = ue_findHomeUnitEnv currentId ue
        ust = homeUnitEnv_units hue
    when False do
      dbg "-----------------------------------"
      dbgp currentId
      dbgp (homeUnitDepends ust)
      -- debugFinder
    names <- liftIO $ listDirectory dir
    let srcs = [dir </> name | name <- names, takeExtension name == ".hs"]
    targets <- mapM (\s -> guessTarget s Nothing Nothing) srcs
    setTargets targets
    hsc_env1 <- getSession
    (errs, graph_nodes) <- liftIO $ downsweep hsc_env1 [] [] True
    let
      mod_graph = mkModuleGraph graph_nodes
    pure (unionManyMessages errs, mod_graph)
  dbgp (text "unit module graph:" <+> showModGraph module_graph)
  liftIO $ updateModuleGraph env.cache module_graph
  pure module_graph
  where
    dir = takeDirectory src

    restoreModuleGraph mg e = e {hsc_mod_graph = unionMG e.hsc_mod_graph mg}

    restoreHug hug e =
      e {hsc_unit_env = e.hsc_unit_env {ue_home_unit_graph = unitEnv_union mergeHugs hug e.hsc_unit_env.ue_home_unit_graph}}

makeModule :: Conf -> [UnitConf] -> Map UnitId String -> UnitMod -> StateT (Set String) IO ()
makeModule Conf {..} units external umod@UnitMod {unit, src, deps} = do
  dbg "---------------------------------------------------------------------------------------------------"
  log <- newLog True
  let env = Env {log, cache, args}
      dbs = concat (fold externalDepDbs)
      envC = env {args = env.args {ghcOptions = dbs ++ env.args.ghcOptions}}
  firstTime <- state \ seen ->
    if Set.member unit seen
    then (False, seen)
    else (True, Set.insert unit seen)
  when firstTime do
    success <- fmap (fromMaybe Nothing) $ liftIO $ withUnitSpecificOptions False env \ env1 specific argv -> do
      (_, withPackageId) <- liftIO $ readMVar cache <&> \case
        Cache {hug}
          | Just h <- hug
          -> fmap dummyLocation <$> adaptHp h (unLoc <$> argv)
          | otherwise
          -> (mempty, argv)
      flip (withGhcInSession env1) (withPackageId ++ fmap dummyLocation dbsM) \ _ -> do
        dbg ""
        dbg (">>> metadata for " ++ unit)
        modifySession $ hscUpdateFlags \ d -> d {ghcMode = MkDepend}
        initializeSessionPlugins
        graph <- loadModuleGraph env1 umod specific
        pure (Just (Just graph))
    liftIO $ readMVar cache >>= \ Cache {..} -> for_ moduleGraph \ mg ->
      dbgp (text "updated module graph:" <+> showModGraph mg)
    void $ case success of
      Just module_graph ->
        liftIO $ withGhcGeneral envC \ specific _ -> do
          -- TODO while this makes the modules discoverable in @depanal@, it also causes @compileHpt@ to break,
          -- when it looks up @unit-main:Err@ for some reason.
          when False do
            dbg ""
            dbg (">>> updating Finder")
            modifySession $ hscUpdateFlags \ d -> d {ghcMode = CompManager}
            initUnit specific
            hsc_env <- getSession
            liftIO $ for_ (mgModSummaries module_graph) \ m -> do
              when (homeUnitId (hsc_home_unit hsc_env) == moduleUnitId (ms_mod m)) do
                dbgp ("add module:" <+> ppr (homeUnitId (hsc_home_unit hsc_env)) <+> ppr (ms_mod_name m))
                void $ addHomeModuleToFinder (hsc_FC hsc_env) (hsc_home_unit hsc_env) (ms_mod_name m) (ms_location m)
            -- debugFinder
          pure (Just ())
      Nothing ->
        liftIO $ throwGhcExceptionIO (ProgramError "Metadata failed")
  result <- liftIO $ withGhcGeneral envC \ specific target -> do
    dbg ""
    dbg (">>> compiling " ++ takeFileName target.get)
    modifySession $ hscUpdateFlags \ d -> d {ghcMode = CompManager}
    when False do
      cache' <- liftIO $ readMVar env.cache
      modifySessionM \ hsc_env -> do
        hsc_FC <- liftIO $ newFinderCache env.cache cache' target
        pure hsc_env {hsc_FC}
      -- debugFinder
    compileHpt specific target
  when (isNothing result) do
      liftIO $ throwGhcExceptionIO (ProgramError "Compile failed")
  dbgs result
  where
    args =
      args0 {
        ghcOptions = args0.ghcOptions ++ fileOptions
      }

    fileOptions =
      [
        "-i",
        -- "-i" ++ takeDirectory src,
        "-this-unit-id",
        unit,
        "-o",
        tmp </> "out" </> (modName ++ ".dyn_o"),
        "-ohi",
        tmp </> "out" </> (modName ++ ".dyn_hi"),
        src
      ] ++
      concat [["-package", dep] | dep <- deps]

    modName = dropExtension (takeFileName src)

    dbsM = concat [["-package-db", db] ++ concat (mapMaybe externalDb externalDeps) | UnitConf {db, externalDeps} <- units]

    externalDepDbs = do
      UnitConf {externalDeps} <- unitConf
      pure (mapMaybe externalDb externalDeps)

    unitConf = find ((== stringToUnitId unit) . (.uid)) units

    externalDb dep = external !? dep <&> \ dir -> ["-package-db", dir]
