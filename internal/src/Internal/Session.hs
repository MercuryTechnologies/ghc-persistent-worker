module Internal.Session where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, readMVar)
import Control.Exception (finally)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList, traverse_)
import Data.IORef (newIORef)
import Data.List (intercalate, isPrefixOf)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import GHC (
  DynFlags (..),
  GeneralFlag (Opt_KeepTmpFiles),
  Ghc,
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
import GHC.Driver.Env (HscEnv (..), hscUpdateFlags)
import GHC.Driver.Errors (printOrThrowDiagnostics)
import GHC.Driver.Errors.Types (DriverMessages, GhcMessage (GhcDriverMessage))
import GHC.Driver.Main (initHscEnv)
import GHC.Driver.Monad (Session (Session), modifySession, unGhc)
import GHC.Runtime.Loader (initializeSessionPlugins)
import GHC.Types.SrcLoc (Located, mkGeneralLocated, unLoc)
import GHC.Utils.Logger (Logger, getLogger, setLogFlags)
import GHC.Utils.Panic (GhcException (UsageError), panic, throwGhcException)
import GHC.Utils.TmpFs (TempDir (..), cleanTempDirs, cleanTempFiles, initTmpFs)
import Internal.State (BinPath (..), WorkerState (..), ModuleArtifacts, Options (..), withCacheMake, withCacheOneshot)
import Internal.Error (handleExceptions)
import Internal.Log (Log (..), logToState)
import Internal.State.Oneshot (OneshotCacheFeatures (..), OneshotState (..))
import Prelude hiding (log)
import System.Environment (setEnv)
import Types.Args (Args (..))
import Types.State (Target (Target))

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
parseFlags :: [Located String] -> Ghc (DynFlags, Logger, [Located String], DriverMessages)
parseFlags argv = do
  dflags0 <- GHC.getSessionDynFlags
  let dflags1 = dflags0 {ghcLink = LinkBinary, verbosity = 0}
  logger1 <- getLogger
  let logger2 = setLogFlags logger1 (initLogFlags dflags1)
  (dflags, fileish_args, dynamicFlagWarnings) <- parseDynamicFlags logger2 dflags1 argv
  pure (dflags, setLogFlags logger2 (initLogFlags dflags), fileish_args, dynamicFlagWarnings)

-- | Parse CLI args and initialize 'DynFlags'.
-- Returns the subset of args that have not been recognized as options.
initDynFlags ::
  DynFlags ->
  Logger ->
  [Located String] ->
  DriverMessages ->
  Ghc (DynFlags, [(String, Maybe Phase)])
initDynFlags dflags0 logger fileish_args dynamicFlagWarnings = do
  liftIO $ printOrThrowDiagnostics logger (initPrintConfig dflags0) (initDiagOpts dflags0) flagWarnings'
  let (dflags1, srcs, objs) = parseTargetFiles dflags0 (map unLoc fileish_args)
  unless (null objs) $ throwGhcException (UsageError ("Targets contain object files: " ++ show objs))
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
  (dflags1, srcs) <- initDynFlags dflags0 logger fileish_args dynamicFlagWarnings
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
  (dflags0, logger, fileish_args, dynamicFlagWarnings) <- parseFlags (argv ++ map instrumentLocation (words state.options.extraGhcOptions))
  result <- prettyPrintGhcErrors logger do
    (dflags, srcs) <- initDynFlags dflags0 logger fileish_args dynamicFlagWarnings
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

-- | When compiling a module, the leftover arguments from parsing @DynFlags@ should be a single source file path.
-- Wrap it in 'Target' or terminate.
ensureSingleTarget :: [(String, Maybe Phase)] -> Ghc Target
ensureSingleTarget = \case
  [(src, Nothing)] -> pure (Target src)
  [(_, phase)] -> panic ("Called worker with unexpected start phase: " ++ show phase)
  args -> panic ("Called worker with multiple targets: " ++ show args)

-- | Run a @Ghc@ program to completion with a fresh clone of the base session, wrapped in a handler operating on a
-- compilation target.
withGhcUsingCache :: (Target -> Ghc a -> Ghc (Maybe b)) -> Env -> (Target -> Ghc a) -> IO (Maybe b)
withGhcUsingCache cacheHandler env prog =
  runSession True env $ withGhcInSession env \ srcs -> do
    target <- ensureSingleTarget srcs
    cacheHandler target do
      initializeSessionPlugins
      prog target

-- | Run a @Ghc@ program to completion with a fresh clone of the base session augmented by some persisted state.
-- This is a compat shim for the multiplex worker.
withGhc :: Env -> (Target -> Ghc (Maybe a)) -> IO (Maybe a)
withGhc env =
  withGhcUsingCache cacheHandler env
  where
    cacheHandler target prog = do
      result <- withCacheOneshot env.log env.args.workerTargetId env.state target do
        res <- prog
        pure do
          a <- res
          pure (Nothing, a)
      pure (snd <$> result)

-- | Run a @Ghc@ program to completion with a fresh clone of the base session augmented by some persisted state.
-- Return the interface and bytecode.
withGhcDefault :: Env -> (Target -> Ghc (Maybe (Maybe ModuleArtifacts, a))) -> IO (Maybe (Maybe ModuleArtifacts, a))
withGhcDefault env =
  withGhcUsingCache (withCacheOneshot env.log env.args.workerTargetId env.state) env

-- | Command line args that have to be stored in the current home unit env.
-- These are specified as a single program argument with their option argument, without whitespace in between.
specificPrefixSwitches :: [String]
specificPrefixSwitches =
  [
    "-i"
  ]

specificPrefixExcludes :: [String]
specificPrefixExcludes =
  [
    "-include-pkg-deps"
  ]

-- | Command line args that have to be stored in the current home unit env.
specificSwitches :: [String]
specificSwitches =
  [
    "-package",
    "-package-id"
  ]

-- | Indicate whether the CLI arg starts with any of the values in 'specificPrefixSwitches'.
isSpecificPrefix :: String -> Bool
isSpecificPrefix arg =
  not (any (`isPrefixOf` arg) specificPrefixExcludes) &&
  any (`isPrefixOf` arg) specificPrefixSwitches

-- | Indicate whether the CLI arg is in 'specificSwitches'.
isSpecific :: String -> Bool
isSpecific =
  flip elem specificSwitches

-- | Separate the command line given by Buck into options pertaining to the target home unit and the rest.
-- Write the rest back to the 'Env' passed to the continuation for processing as global args, and pass the home unit
-- specific args as the second argument to the continuation.
--
-- @-hide-all-packages@ is removed entirely, which may be obsolete.
-- @-this-unit-id@ is added to both parts, since the global session is always initialized with a default session.
withUnitSpecificOptions :: Bool -> Env -> (Env -> [String] -> [Located String] -> Ghc (Maybe a)) -> IO (Maybe a)
withUnitSpecificOptions reuse env use =
  runSession reuse env1 $ use env1 specific
  where
    env1 = env {args = env.args {ghcOptions = general}}
    (general, specific) = spin ([], []) env.args.ghcOptions

    spin (g, s) = \case
      [] -> (reverse g, reverse s)
      "-hide-all-packages" : rest
        -> spin (g, s) rest
      "-this-unit-id" : uid : rest
        -> spin (uid : "-this-unit-id" : g, uid : "-this-unit-id" : s) rest
      switch : arg : rest
        | isSpecific switch
        -> spin (g, arg : switch : s) rest
      arg : rest
        | isSpecificPrefix arg
        -> spin (g, arg : s) rest
        | otherwise
        -> spin (arg : g, s) rest

-- | Run a GHC session with multiple home unit support, separating the CLI args for the current unit from the rest.
withGhcInSessionMhu ::
  Env ->
  ([String] -> [(String, Maybe Phase)] -> Ghc (Maybe a)) ->
  IO (Maybe a)
withGhcInSessionMhu env prog =
  withUnitSpecificOptions True env \ env1 specific -> withGhcInSession env1 (prog specific)

-- | Like @withGhcInSessionMhu@, but wrap with the given function operating on the current target for caching purposes.
withGhcUsingCacheMhu ::
  (Target -> Ghc a -> Ghc (Maybe b)) ->
  Env ->
  ([String] -> Target -> Ghc a) ->
  IO (Maybe b)
withGhcUsingCacheMhu cacheHandler env prog =
  withGhcInSessionMhu env \ specific srcs -> do
    target <- ensureSingleTarget srcs
    cacheHandler target do
      initializeSessionPlugins
      prog specific target

-- | Like @withGhcUsingCacheMhu@, using the default cache handler @withCache@.
withGhcMhu :: Env -> ([String] -> Target -> Ghc (Maybe a)) -> IO (Maybe a)
withGhcMhu env f =
  withGhcUsingCacheMhu cacheHandler env f
  where
    cacheHandler _ prog = do
      result <- withCacheMake env.log env.state do
        res <- prog
        pure do
          a <- res
          pure (Nothing, a)
      pure (snd <$> result)
