module TestMake where

import Control.Concurrent (readMVar)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (StateT, state)
import Data.Foldable (find, fold, for_)
import Data.Functor ((<&>))
import Data.Map.Strict (Map, (!?))
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC (DynFlags (..), Ghc, GhcException (..), GhcMode (..), getSession, guessTarget, setTargets, unLoc)
import GHC.Driver.Env (HscEnv (..), hscUpdateFlags)
import GHC.Driver.Make (depanal)
import GHC.Driver.Monad (modifySession, withTempSession)
import GHC.Runtime.Loader (initializeSessionPlugins)
import GHC.Unit (UnitId, stringToUnitId)
import GHC.Unit.Env (UnitEnv (..), unitEnv_union)
import GHC.Unit.Module.Graph (unionMG)
import GHC.Utils.Outputable (text, (<+>))
import GHC.Utils.Panic (throwGhcExceptionIO)
import Internal.Args (Args (..))
import Internal.Cache (Cache (..), mergeHugs, updateModuleGraph)
import Internal.CompileHpt (adaptHp, compileHpt)
import Internal.Debug (showModGraph)
import Internal.Log (dbg, dbgp, dbgs, newLog)
import Internal.Session (Env (..), dummyLocation, runSession, withGhcGeneral, withGhcInSession)
import Prelude hiding (log)
import System.Directory (listDirectory)
import System.FilePath (dropExtension, takeDirectory, takeExtension, takeFileName, (</>))
import TestSetup (Conf (..), UnitConf (UnitConf, db, externalDeps, uid), UnitMod (..))

loadModuleGraph :: Env -> UnitMod -> Ghc ()
loadModuleGraph env UnitMod {src} = do
  cache <- liftIO $ readMVar env.cache
  module_graph <- withTempSession (maybe id restoreHug cache.hug . maybe id restoreModuleGraph cache.moduleGraph) do
    mg <- hsc_mod_graph <$> getSession
    dbgp ("active graph: " <+> showModGraph mg)
    names <- liftIO $ listDirectory dir
    let srcs = [dir </> name | name <- names, takeExtension name == ".hs"]
    targets <- mapM (\s -> guessTarget s Nothing Nothing) srcs
    setTargets targets
    depanal [] True
  dbgp (text "unit module graph:" <+> showModGraph module_graph)
  liftIO $ updateModuleGraph env.cache module_graph
  where
    dir = takeDirectory src

    restoreModuleGraph mg e = e {hsc_mod_graph = unionMG e.hsc_mod_graph mg}

    restoreHug hug e =
      e {hsc_unit_env = e.hsc_unit_env {ue_home_unit_graph = unitEnv_union mergeHugs hug e.hsc_unit_env.ue_home_unit_graph}}

makeModule :: Conf -> [UnitConf] -> Map UnitId String -> UnitMod -> StateT (Set String) IO ()
makeModule Conf {..} units external umod@UnitMod {unit, src, deps} = do
  dbg "---------------------------------"
  log <- newLog True
  let env = Env {log, cache, args}
  firstTime <- state \ seen ->
    if Set.member unit seen
    then (False, seen)
    else (True, Set.insert unit seen)
  when firstTime do
    success <- fmap (fromMaybe False) $ liftIO $ runSession False env \ argv -> do
      dbg ("metadata for " ++ unit)
      (_, withPackageId) <- liftIO $ readMVar cache <&> \case
        Cache {hug}
          | Just h <- hug
          -> fmap dummyLocation <$> adaptHp h (unLoc <$> argv)
          | otherwise
          -> (mempty, argv)
      let dbs = concat [["-package-db", db] ++ concat (mapMaybe externalDb externalDeps) | UnitConf {db, externalDeps} <- units]
      flip (withGhcInSession env) (withPackageId ++ fmap dummyLocation dbs) \ _ -> do
        modifySession $ hscUpdateFlags \ d -> d {ghcMode = MkDepend}
        initializeSessionPlugins
        loadModuleGraph env umod
        pure (Just True)
    unless success do
      liftIO $ throwGhcExceptionIO (ProgramError "Metadata failed")
    mb_mg <- liftIO $ readMVar cache <&> \ Cache {..} -> moduleGraph
    for_ mb_mg \ mg ->
      dbgp (text "updated module graph:" <+> showModGraph mg)
  let dbs = concat (fold externalDepDbs)
  result <- liftIO $ withGhcGeneral env {args = env.args {ghcOptions = dbs ++ env.args.ghcOptions}} \ specific target -> do
    modifySession $ hscUpdateFlags \ d -> d {ghcMode = CompManager}
    result <- compileHpt specific target
    pure result
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

    externalDepDbs = do
      UnitConf {externalDeps} <- unitConf
      pure (mapMaybe externalDb externalDeps)

    unitConf = find ((== stringToUnitId unit) . (.uid)) units

    externalDb dep = external !? dep <&> \ dir -> ["-package-db", dir]
