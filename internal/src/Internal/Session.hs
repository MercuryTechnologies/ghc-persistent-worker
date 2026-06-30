{-# LANGUAGE CPP #-}

module Internal.Session where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, readMVar)
import Control.Exception (finally)
import Control.Monad (foldM, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.IORef (newIORef)
import Data.Maybe (fromMaybe)
import GHC (
  DynFlags (..),
  GeneralFlag (Opt_KeepTmpFiles),
  Ghc,
  Phase,
  getSession,
  getSessionDynFlags,
  gopt,
  popLogHookM,
  prettyPrintGhcErrors,
  pushLogHookM,
  setSessionDynFlags,
  withSignalHandlers,
  )
import GHC.Driver.Env (HscEnv (..), hscSetActiveUnitId)
import GHC.Driver.Main (initHscEnv)
import GHC.Driver.Monad (Session (Session), modifySession, unGhc)
import GHC.Runtime.Loader (initializeSessionPlugins)
import GHC.Types.SrcLoc (Located)
import GHC.Unit (moduleUnitId)
import GHC.Utils.Logger (getLogger)
import GHC.Utils.Outputable (ppr, text, (<+>))
import GHC.Utils.Panic (panic, pprPanic)
import GHC.Utils.TmpFs (TempDir (..), cleanTempDirs, cleanTempFiles, initTmpFs)
import Internal.Cache.Hpt (loadCachedDeps, loadHomeUnit)
import Internal.Compat.GHC914 (hscSetModuleGraph)
import Internal.DynFlags (
  buckLocation,
  initDynFlags,
  instrumentLocation,
  mkTargetAsInterpreted,
  parseFlags,
  setupPath,
  updateGlobalFlags,
  )
import Internal.Env (withDebugLog)
import Internal.Error (handleExceptions)
import Internal.Log (logDebugD)
import Internal.State (withState)
import Prelude hiding (log)
import System.OsPath.Extra (OsPath, fromOsPath, toOsPath)
import Types.Args (Args (..))
import Types.BuckArgs (IsInterpreted (Interpreted))
import Types.Env (Env (..))
import Types.Log (Logger (..))
import Types.State (Options (..), WorkerState (..))
import Types.State.Make (MakeState (..))
import Types.Target (ModuleTarget (..), Target (Target), TargetSpec (..))
import Data.Function ((&))

setTempDir :: OsPath -> HscEnv -> HscEnv
setTempDir dir = updateGlobalFlags \ dflags -> dflags {tmpDir = TempDir (fromOsPath dir)}

-- | Run a program with fresh 'DynFlags' constructed from command line args.
-- Passes the flags and the unprocessed args to the callback, which usually consist of the file or module names intended
-- for compilation.
-- In a Buck compile step these should always be a single path, but in the metadata step they enumerate an entire unit.
--
-- TODO Get rid of @prettyPrintGhcErrors@
-- TODO Why are we popping the log hook here???
withDynFlags :: Env -> (DynFlags -> [(String, Maybe Phase)] -> Ghc a) -> [Located String] -> Ghc a
withDynFlags env prog argv = do
  state <- liftIO $ readMVar env.state
  dflags0 <- GHC.getSessionDynFlags
  logger0 <- getLogger
  (dflags1, logger, fileish_args, dynamicFlagWarnings) <- liftIO $ parseFlags dflags0 logger0 (argv ++ map instrumentLocation (words state.options.extraGhcOptions))
  result <- prettyPrintGhcErrors logger do
    (dflags, srcs) <- liftIO $ initDynFlags dflags1 logger fileish_args dynamicFlagWarnings
    prog dflags srcs
  result <$ popLogHookM

-- | Run a program with a fresh session constructed from command line args.
-- Passes the unprocessed args to the callback, which usually consist of the file or module names intended for
-- compilation.
-- In a Buck compile step these should always be a single path, but in the metadata step they enumerate an entire unit.
withGhcInSession :: Env -> ([(String, Maybe Phase)] -> Ghc a) -> [Located String] -> Ghc a
withGhcInSession env prog =
  withDynFlags env \ dflags srcs -> do
    setSessionDynFlags dflags
    prog srcs

-- | Create a base session and store it in the cache.
-- On subsequent calls, return the session cached in the 'WorkerState'.
--
-- Create a new @TmpFs@ to avoid keeping old entries around after Buck deletes the directories.
ensureSession :: MVar WorkerState -> Args -> IO HscEnv
ensureSession stateVar args =
  modifyMVar stateVar \ state -> do
    newEnv <- maybe (initHscEnv args.topdir) prepReused state.baseSession
    pure (state {baseSession = Just newEnv}, newEnv)
  where
    prepReused hsc_env = do
      hsc_tmpfs <- initTmpFs
      pure hsc_env {hsc_tmpfs}

runGhc :: Session -> Ghc a -> IO a
runGhc = flip unGhc

initSession :: Env -> IO Session
initSession Env {args, state, log} = do
  modifyMVar_ state (setupPath args.binPath)
  hsc_env <- ensureSession state args
  session <- Session <$> newIORef hsc_env
  runGhc session do
    traverse_ (modifySession . setTempDir) args.tempDir
    pushLogHookM (const log.ghcAction)
  pure session

-- TODO Remove signal handler.
runWithSession ::
  Env ->
  Session ->
  Ghc (Maybe a) ->
  IO (Maybe a)
runWithSession Env {..} session prog =
  runGhc session $ withSignalHandlers do
    handleExceptions log Nothing prog

cleanupSession :: Session -> IO ()
cleanupSession session =
  flip unGhc session do
    hsc_env <- getSession
    liftIO $ unless (gopt Opt_KeepTmpFiles (hsc_dflags hsc_env)) do
      let tmpfs = hsc_tmpfs hsc_env
          logger = hsc_logger hsc_env
      cleanTempFiles logger tmpfs
      cleanTempDirs logger tmpfs

-- | Run a @Ghc@ program to completion with a fresh clone of the base session.
-- See 'ensureSession' for @reuse@.
--
-- Delete all temporary files on completion.
runSession :: Env -> ([Located String] -> Ghc (Maybe a)) -> IO (Maybe a)
runSession env prog = do
  session <- initSession env
  finally (runWithSession env session (prog locatedArgs)) (cleanupSession session)
  where
    locatedArgs = map buckLocation env.args.ghcOptions

-- | Parse the CLI arguments stored in the 'Env' and run a @Ghc@ program with the resulting 'DynFlags'.
simpleSession :: Env -> Ghc a -> IO (Maybe a)
simpleSession env ma =
  runSession env (withGhcInSession env (const (Just <$> ma)))

-- | Run a @Ghc@ program with a fresh log and print all messages to stderr afterwards.
sessionWithDebugLog :: MVar WorkerState -> Args -> (Env -> [Located String] -> Ghc a) -> IO (Maybe a)
sessionWithDebugLog state args use =
  withDebugLog state args \ env ->
    runSession env (fmap Just . use env)

-- | Parse the CLI arguments stored in the 'Env', run a @Ghc@ program with the resulting 'DynFlags', and print all
-- messages to stderr afterwards.
simpleSessionWithDebugLog :: MVar WorkerState -> Args -> Ghc a -> IO (Maybe a)
simpleSessionWithDebugLog state args ma =
  sessionWithDebugLog state args \ env -> withGhcInSession env (const ma)

-- | When compiling a source target, the leftover arguments from parsing @DynFlags@ should be a single source file path.
-- Wrap it in 'Target' or terminate.
ensureSingleTarget :: [(String, Maybe Phase)] -> Ghc Target
ensureSingleTarget = \case
  [(src, Nothing)] -> pure (Target $ toOsPath src)
  [(_, phase)] -> panic ("Called worker with unexpected start phase: " ++ show phase)
  args -> panic ("Called worker with multiple source targets: " ++ show args)

-- | When compiling a module target, there should not be any leftover arguments.
ensureNoArgs :: [(String, Maybe Phase)] -> Ghc ()
ensureNoArgs = \case
  [] -> pure ()
  args -> pprPanic "Extraneous arguments for GHC in module graph mode" (text (unwords (fst <$> args)))

-- | Run a @Ghc@ program to completion with a fresh clone of the base session.
-- Passes the args GHC did not process to a handler for extracting the compilation target.
withGhc ::
  (Env -> [(String, Maybe Phase)] -> (t -> Ghc a) -> Ghc (Maybe b)) ->
  Env ->
  (t -> Ghc a) ->
  IO (Maybe b)
withGhc targetWrapper env prog =
  runSession env $ withGhcInSession env \ srcs ->
    targetWrapper env srcs \ target -> do
      initializeSessionPlugins
      prog target

-- | Run a @Ghc@ program to completion with a fresh clone of the base session.
-- Extracts a single source file target from the leftover args and passes it to a cache wrapper before running the main
-- program.
withGhcSource ::
  (Target -> Logger -> MVar WorkerState -> Ghc a -> Ghc (Maybe b)) ->
  Env ->
  (Target -> Ghc a) ->
  IO (Maybe b)
withGhcSource cacheWrapper =
  withGhc \ env srcs run -> do
    target <- ensureSingleTarget srcs
    logDebugD env.log (text "Compiling source target" <+> ppr target)
    cacheWrapper target env.log env.state do
      run target

-- | Like @withGhcSource@, using the make cache handler @withCacheMake@.
withGhcMakeSource :: Env -> (Target -> Ghc (Maybe a)) -> IO (Maybe a)
withGhcMakeSource =
  withGhcSource \ _ logger stateVar ma -> withState logger stateVar pure ma

-- | Run a GHC session with multiple home unit support for a module target.
--
-- Before compilation, ensure that the module's home unit is present in the session's unit state and the session's home
-- package tables contain the module's dependencies, restoring them from cache if necessary.
-- Since this mode does not process any new command line arguments, we set the active home unit manually.
withGhcMakeModule ::
  IsInterpreted ->
  ModuleTarget ->
  Env ->
  (TargetSpec -> Ghc (Maybe a)) ->
  IO (Maybe a)
withGhcMakeModule interp target =
  withGhc \ env srcs run -> do
    dflags0 <- getSessionDynFlags
    ensureNoArgs srcs
    logDebugD env.log (text "Compiling module target" <+> ppr target)
    withState env.log env.state (setup env dflags0) do
      initializeSessionPlugins
      run (targetSpec target)
  where
    setup env dflags0 (state0, hsc_env0) =
      foldM @[] (&) (state0, hsc_env0) [
        pure . fmap setTarget,
        restoreCachedHomeUnit env dflags0,
        setSessionModuleGraph,
        setActiveUnit,
        restoreCachedModules env
      ]

    restoreCachedHomeUnit env dflags0 =
      maybeArg env.args.homeUnit $
      loadHomeUnit env.log dflags0 env.args.features (moduleUnitId target.mod)

    setSessionModuleGraph (state, hsc_env) = pure (state, hscSetModuleGraph state.make.moduleGraph hsc_env)

    setActiveUnit (state, hsc_env) = pure (state, hscSetActiveUnitId (moduleUnitId target.mod) hsc_env)

    restoreCachedModules env =
      maybeArg env.args.cachedDeps (loadCachedDeps env.log interp)

    maybeArg :: Maybe a -> (b -> a -> IO b) -> b -> IO b
    maybeArg arg f z = fromMaybe z <$> traverse (liftIO . f z) arg

    (targetSpec, setTarget)
      | Interpreted <- interp = (TargetModuleInterp, mkTargetAsInterpreted target.mod)
      | otherwise = (TargetModule, id)
