-- | Extra native libraries of home units, the @-l@ options of their flags.
--
-- GHC loads them when the loader starts: @loadCmdLineLibs'@ preloads the
-- @-l@ libraries of every home unit of the session.
-- In the worker, the session holds every unit the worker has seen, and one
-- loader serves the whole build, so a compile would load the libraries of
-- units it has nothing to do with, and fail if one of them cannot be
-- loaded.
-- The worker keeps the @-l@ options out of the unit flags in its session
-- ('stripLibraryInputs'), records them per unit ('recordExtraLibs'), and
-- loads them itself when a compile uses the loader ('prepareLoader' for a
-- plugin, 'withLibraryLoading' for code GHC evaluates): those of the
-- compiling unit and of its home-unit dependencies.
-- That is what GHC would load in a session that holds only those units.
--
-- We are careful to avoid depending on @loadCmdLineLibs'@, which would load the
-- extra libraries of any unit in the session, even if they are unrelated to the
-- current compilation request.
module Internal.State.ExtraLibs (
  stripLibraryInputs,
  recordExtraLibs,
  usesPlugins,
  homeUnitClosure,
  loadUnitLibraries,
  prepareLoader,
  withLibraryLoading,
  initializePlugins,
) where

import Control.Concurrent (MVar)
import Control.Monad (foldM, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Foldable (for_, toList)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import GHC.Driver.DynFlags (DynFlags (..), targetPlatform)
import GHC.Driver.Env (hscActiveUnitId, hscSetActiveUnitId, hsc_all_home_unit_ids, hsc_units)
import GHC.Driver.Env.Types (HscEnv (..))
import GHC.Driver.Hooks (Hooks (..))
import GHC.Driver.Main (hscCompileCoreExpr')
import GHC.Driver.Monad (Ghc, getSession)
import GHC.Platform (platformSOName)
import GHC.Runtime.Interpreter (Interp, initObjLinker, loadDLL)
import GHC.Runtime.Loader (initializeSessionPlugins)
import GHC.Unit (UnitId)
import GHC.Unit.State (UnitState (..))
import GHC.Utils.CliOption (Option (..))
import GHC.Utils.Outputable (text, (<+>))
import Internal.Error (workerErrorIO)
import Internal.State (modifyMakeState)
import System.Directory (findFile)
import Types.State (WorkerState)
import Types.State.Make (LibLoadState (..), MakeState (..))

-- | Drops flags starting with @-l@.
stripLibraryInputs :: DynFlags -> DynFlags
stripLibraryInputs dflags =
  dflags {ldInputs = [i | i <- dflags.ldInputs, not (isLibrary i)]}
  where
    isLibrary = \case
      Option ('-' : 'l' : _) -> True
      _ -> False

-- | Record in the 'MakeState' that @unit@ requires the libraries specified in
-- @ldInputs dflags@.
--
-- The records are then used in 'loadUnitLibraries'.
--
-- TODO: this ad hoc extraction of extra library dependencies should be
-- replaced by a proper specification from the build system rules, recorded
-- in a file, preferably the build plan file.
recordExtraLibs :: UnitId -> DynFlags -> MakeState -> MakeState
recordExtraLibs unit dflags state =
  state {extraLib = state.extraLib {requested}}
  where
    requested = M.insert unit (dflags.libraryPaths, libs) state.extraLib.requested
    libs = [lib | Option ('-' : 'l' : lib) <- dflags.ldInputs]

-- | Whether a compile with these flags loads a plugin, which starts GHC's
-- loader before the compile.
--
-- Code that GHC evaluates during the compile, splices and annotations, is
-- covered by 'withLibraryLoading' instead, whatever the flags.
usesPlugins :: DynFlags -> Bool
usesPlugins dflags =
  not (null dflags.pluginModNames)
  || not (null dflags.externalPluginSpecs)

-- | The active unit and its transitive home-unit dependencies in
-- topological order, each after its dependencies.
--
-- This is the order in which GHC's @loadCmdLineLibs'@ visits units.
homeUnitClosure :: HscEnv -> [UnitId]
homeUnitClosure hsc_env =
  reverse (snd (visit (S.empty, []) (hscActiveUnitId hsc_env)))
  where
    homeUnits = hsc_all_home_unit_ids hsc_env

    -- @S.fromList acc `S.isSubsetOf` done@
    --
    -- There is an implicit path @p@ leading from the active unit to @unit@.
    --
    -- @done@ contains the units in @p@ or at the left of @p@.
    --
    -- @acc@ contains all the units at the left of @p@ in reverse
    -- topological order.
    visit (done, acc) unit
      | unit `S.member` done || not (unit `S.member` homeUnits) = (done, acc)
      | otherwise =
        let deps = toList (homeUnitDepends (hsc_units (hscSetActiveUnitId unit hsc_env)))
            (done', acc') = foldl visit (S.insert unit done, acc) deps
         in (done', unit : acc')

-- | Load the recorded libraries of the given units, in order, each library
-- at most once for the whole worker.
loadUnitLibraries :: MVar WorkerState -> HscEnv -> Interp -> [UnitId] -> IO ()
loadUnitLibraries stateVar hsc_env interp units =
  for_ units \ unit ->
    modifyMakeState stateVar \ make ->
      case M.lookup unit make.extraLib.requested of
        Nothing -> pure (make, ())
        Just (lib_paths, libs) -> do
          loaded <- foldM (load lib_paths) make.extraLib.loaded libs
          pure (make {extraLib = make.extraLib {loaded}}, ())
  where
    -- Only the RTS linker has to be running to load a library, not GHC's
    -- loader, whose state GHC still initializes at its own first use.
    -- Starting the RTS linker again is a no-op.
    load lib_paths loaded lib
      | lib `S.member` loaded = pure loaded
      | otherwise = do
        initObjLinker interp
        S.insert lib loaded <$ loadLibrary hsc_env interp lib_paths lib

-- | Loads the first shared library with name @lib@ that is found
-- in any of the directories in @lib_paths@. Fails if no such library
-- is found.
loadLibrary :: HscEnv -> Interp -> [FilePath] -> String -> IO ()
loadLibrary hsc_env interp lib_paths lib = do
  findFile lib_paths so_name >>= \case
    Nothing -> emitError ("Library not found: " <+> text so_name)
    Just so_file ->
      loadDLL interp so_file >>= \case
        Left err -> emitError (text err)
        Right _ -> pure ()
  where
    so_name = platformSOName (targetPlatform hsc_env.hsc_dflags) lib

    emitError msg = workerErrorIO hsc_env ("Loading DLL error:" <+> msg)

-- | Like GHC's 'initializeSessionPlugins', but loads the extra libraries a
-- plugin may need in advance.
initializePlugins :: MVar WorkerState -> Ghc ()
initializePlugins stateVar = do
  hsc_env <- getSession
  liftIO (prepareLoader stateVar hsc_env hsc_env.hsc_dflags)
  initializeSessionPlugins

-- | Before a compile with these flags that can use the loader, load the
-- libraries of the compiling unit and its home-unit dependencies.
--
-- When none of these units has a library left to load, this does nothing,
-- and GHC's loader starts as it would without the worker.
prepareLoader :: MVar WorkerState -> HscEnv -> DynFlags -> IO ()
prepareLoader stateVar hsc_env dflags =
  when (usesPlugins dflags) do
    for_ hsc_env.hsc_interp \ interp ->
      loadUnitLibraries stateVar hsc_env interp (homeUnitClosure hsc_env)

-- | Sets a hook to load libraries before GHC evaluates code during
-- compilation: those of the compiling unit and its home-unit dependencies.
--
-- GHC evaluates code when running splices and when evaluating annotations.
--
-- Only the first call of the hook loads libraries. Every call then goes on to
-- the hook already set in the session, or to GHC's own function without one,
-- so later calls behave as if this hook had not been set.
withLibraryLoading :: MVar WorkerState -> HscEnv -> IO HscEnv
withLibraryLoading stateVar hsc_env = do
  pending <- newIORef True
  let compileCoreExpr env loc expr = do
        first <- atomicModifyIORef' pending (\ p -> (False, p))
        when first do
          for_ env.hsc_interp \ interp ->
            loadUnitLibraries stateVar env interp (homeUnitClosure env)
        previous env loc expr
  pure hsc_env {hsc_hooks = hooks {hscCompileCoreExprHook = Just compileCoreExpr}}
  where
    hooks = hsc_env.hsc_hooks
    -- The hook of @hsc_env@, or GHC's own function without one. It is applied
    -- to @env@, the session GHC hands the hook when it evaluates, and does not
    -- call this hook again: only @hscCompileCoreExpr@ dispatches through the
    -- hooks, and @hscCompileCoreExpr'@ never reads them.
    previous = fromMaybe hscCompileCoreExpr' hooks.hscCompileCoreExprHook
