{-# LANGUAGE CPP #-}

module Internal.Session where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, readMVar)
import Control.Exception (finally)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList, traverse_)
import Data.IORef (newIORef)
import Data.List (intercalate)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import GHC (
  DynFlags (..),
  GeneralFlag (Opt_KeepTmpFiles),
  Ghc,
  GhcException (..),
  GhcLink (LinkBinary),
  Phase,
  getSession,
  getSessionDynFlags,
  gopt,
  parseDynamicFlags,
  parseTargetFiles,
  popLogHookM,
  prettyPrintGhcErrors,
  pushLogHookM,
  setSessionDynFlags,
  withSignalHandlers,
  )
import GHC.Driver.Config.Diagnostic (initDiagOpts, initPrintConfig)
import GHC.Driver.Config.Logger (initLogFlags)
import GHC.Driver.Env (HscEnv (..), hscSetActiveUnitId, hscUpdateFlags)
import GHC.Driver.Errors (printOrThrowDiagnostics)
import GHC.Driver.Errors.Types (DriverMessages, GhcMessage (GhcDriverMessage))
import GHC.Driver.Main (initHscEnv)
import GHC.Driver.Monad (Session (Session), modifySession, modifySessionM, unGhc)
import GHC.Runtime.Loader (initializeSessionPlugins)
import GHC.Types.SrcLoc (Located, mkGeneralLocated, unLoc)
import GHC.Unit (moduleUnitId)
import GHC.Utils.Logger (Logger, getLogger, setLogFlags)
import GHC.Utils.Outputable (ppr, text, (<+>))
import GHC.Utils.Panic (panic, throwGhcExceptionIO, pprPanic)
import GHC.Utils.TmpFs (TempDir (..), cleanTempDirs, cleanTempFiles, initTmpFs)
import Internal.Cache.Hpt (loadCachedDeps)
import Internal.Error (handleExceptions)
import Internal.Log (Log (..), logDebugD, logToState)
import Internal.State (BinPath (..), Options (..), WorkerState (..), withCacheMake, withCacheOneshot)
import Internal.State.Oneshot (OneshotCacheFeatures (..), OneshotState (..))
import Prelude hiding (log)
import System.Environment (setEnv)
import Types.Args (Args (..))
import Types.Target (ModuleTarget (..), Target (Target), TargetSpec (..))

-- | Data used by a single worker request session, consisting of a logger, shared state, and request arguments.
data Env =
  Env {
    -- | Logger used to receive messages from GHC and relay them to Buck.
    log :: MVar Log,

    -- | The entirety of the persistent state of a worker thats's shared across sessions.
    state :: MVar WorkerState,

    -- | Preprocessed command line args from Buck.
    args :: Args
  }

-- | Add all the directories passed by Buck in @--bin-path@ options to the global @$PATH@.
-- Although Buck intends these to be module specific, all subsequent compile jobs will see all previous jobs' entries,
-- since we only have one process environment.
setupPath :: Args -> WorkerState -> IO WorkerState
setupPath args old = do
  setEnv "PATH" (intercalate ":" (toList path.extra ++ maybeToList path.initial))
  pure new
  where
    path = new.path
    new = old {path = old.path {extra}}
    extra
      | Just cur <- nonEmpty args.binPath
      = Set.union old.path.extra (Set.fromList (toList cur))
      | otherwise
      = old.path.extra

setTempDir :: String -> HscEnv -> HscEnv
setTempDir dir = hscUpdateFlags \ dflags -> dflags {tmpDir = TempDir dir}

buckLocation :: a -> Located a
buckLocation = mkGeneralLocated "by Buck2"

instrumentLocation :: a -> Located a
instrumentLocation = mkGeneralLocated "by instrument"

-- | Parse command line flags into @DynFlags@ and set up the logger. Extracted from GHC.
-- Returns the subset of args that have not been recognized as options.
parseFlags ::
  DynFlags ->
  Logger ->
  [Located String] ->
  IO (DynFlags, Logger, [Located String], DriverMessages)
parseFlags dflags0 logger0 argv = do
  let dflags1 = dflags0 {ghcLink = LinkBinary, verbosity = 0}
  let logger1 = setLogFlags logger0 (initLogFlags dflags1)
  (dflags, fileish_args, dynamicFlagWarnings) <- parseDynamicFlags logger1 dflags1 argv
  pure (dflags, setLogFlags logger1 (initLogFlags dflags), fileish_args, dynamicFlagWarnings)

-- | Parse CLI args and initialize 'DynFlags'.
-- Returns the subset of args that have not been recognized as options.
initDynFlags ::
  DynFlags ->
  Logger ->
  [Located String] ->
  DriverMessages ->
  IO (DynFlags, [(String, Maybe Phase)])
initDynFlags dflags0 logger fileish_args dynamicFlagWarnings = do
  printOrThrowDiagnostics logger (initPrintConfig dflags0) (initDiagOpts dflags0) flagWarnings'
  let (dflags1, srcs, objs) = parseTargetFiles dflags0 (map unLoc fileish_args)
  unless (null objs) $ throwGhcExceptionIO (UsageError ("Targets contain object files: " ++ show objs))
  pure (dflags1, srcs)
  where
    flagWarnings' = GhcDriverMessage <$> dynamicFlagWarnings

-- | Parse CLI args and set up the GHC session.
-- Returns the subset of args that have not been recognized as options.
initGhc ::
  DynFlags ->
  Logger ->
  [Located String] ->
  DriverMessages ->
  Ghc [(String, Maybe Phase)]
initGhc dflags0 logger fileish_args dynamicFlagWarnings = do
  (dflags1, srcs) <- liftIO $ initDynFlags dflags0 logger fileish_args dynamicFlagWarnings
  setSessionDynFlags dflags1
  pure srcs

-- | Run a program with fresh 'DynFlags' constructed from command line args.
-- Passes the flags and the unprocessed args to the callback, which usually consist of the file or module names intended
-- for compilation.
-- In a Buck compile step these should always be a single path, but in the metadata step they enumerate an entire unit.
withDynFlags :: Env -> (DynFlags -> [(String, Maybe Phase)] -> Ghc a) -> [Located String] -> Ghc a
withDynFlags env prog argv = do
  let !log = env.log
  pushLogHookM (const (logToState log))
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
-- On subsequent calls, return the cached session, unless the cache is disabled or @reuse@ is true.
-- This will at some point be replaced by more deliberate methods.
--
-- When reusing the base session, create a new @TmpFs@ to avoid keeping old entries around after Buck deletes the
-- directories.
ensureSession :: Bool -> MVar WorkerState -> Args -> IO HscEnv
ensureSession reuse stateVar args =
  modifyMVar stateVar \ state -> do
    if state.oneshot.features.enable && reuse
    then do
      newEnv <- maybe (initHscEnv args.topdir) prepReused state.baseSession
      pure (state {baseSession = Just newEnv}, newEnv)
    else do
      newEnv <- initHscEnv args.topdir
      pure (state, newEnv)
  where
    prepReused hsc_env = do
      hsc_tmpfs <- initTmpFs
      pure hsc_env {hsc_tmpfs}

-- | Run a @Ghc@ program to completion with a fresh clone of the base session.
-- See 'ensureSession' for @reuse@.
--
-- Delete all temporary files on completion.
runSession :: Bool -> Env -> ([Located String] -> Ghc (Maybe a)) -> IO (Maybe a)
runSession reuse Env {log, args, state} prog = do
  modifyMVar_ state (setupPath args)
  hsc_env <- ensureSession reuse state args
  session <- Session <$> newIORef hsc_env
  finally (run session) (cleanup session)
  where
    run session =
      flip unGhc session $ withSignalHandlers do
        traverse_ (modifySession . setTempDir) args.tempDir
        handleExceptions log Nothing (prog (map buckLocation args.ghcOptions))

    cleanup session =
      flip unGhc session do
        hsc_env <- getSession
        liftIO $ unless (gopt Opt_KeepTmpFiles (hsc_dflags hsc_env)) do
          let tmpfs = hsc_tmpfs hsc_env
              logger = hsc_logger hsc_env
          cleanTempFiles logger tmpfs
          cleanTempDirs logger tmpfs

-- | When compiling a source target, the leftover arguments from parsing @DynFlags@ should be a single source file path.
-- Wrap it in 'Target' or terminate.
ensureSingleTarget :: [(String, Maybe Phase)] -> Ghc Target
ensureSingleTarget = \case
  [(src, Nothing)] -> pure (Target src)
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
  runSession True env $ withGhcInSession env \ srcs ->
    targetWrapper env srcs \ target -> do
      initializeSessionPlugins
      prog target

-- | Run a @Ghc@ program to completion with a fresh clone of the base session.
-- Extracts a single source file target from the leftover args and passes it to a cache wrapper before running the main
-- program.
withGhcSource ::
  (Target -> MVar Log -> MVar WorkerState -> Ghc a -> Ghc (Maybe b)) ->
  Env ->
  (Target -> Ghc a) ->
  IO (Maybe b)
withGhcSource cacheWrapper =
  withGhc \ env srcs run -> do
    target <- ensureSingleTarget srcs
    logDebugD env.log (text "Compiling source target" <+> ppr target)
    cacheWrapper target env.log env.state do
      run target

-- | Like @withGhcSource@, using the oneshot cache handler @withCacheOneshot@.
withGhcOneshotSource :: Env -> (Target -> Ghc (Maybe a)) -> IO (Maybe a)
withGhcOneshotSource env =
  withGhcSource (withCacheOneshot env.args.workerTargetId) env

-- | Like @withGhcSource@, using the make cache handler @withCacheMake@.
withGhcMakeSource :: Env -> (Target -> Ghc (Maybe a)) -> IO (Maybe a)
withGhcMakeSource =
  withGhcSource (const withCacheMake)

-- | Run a GHC session with multiple home unit support for a module target.
--
-- Before compilation, ensure that the session's home package tables contain the module's dependencies, restoring them
-- from cache if necessary.
-- Since this mode does not process any new command line arguments, we set the active home unit manually.
withGhcMakeModule ::
  ModuleTarget ->
  Env ->
  (TargetSpec -> Ghc (Maybe a)) ->
  IO (Maybe a)
withGhcMakeModule target =
  withGhc \ env srcs run -> do
    ensureNoArgs srcs
    logDebugD env.log (text "Compiling module target" <+> ppr target)
    withCacheMake env.log env.state do
      modifySession (hscSetActiveUnitId (moduleUnitId target.mod))
      traverse_ (modifySessionM . loadCachedDeps env.log) env.args.cachedDeps
      initializeSessionPlugins
      run (TargetModule target)
