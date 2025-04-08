module Internal.Session where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_)
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
  Ghc,
  GhcLink (LinkBinary),
  GhcMode (OneShot),
  Phase,
  getSessionDynFlags,
  parseDynamicFlags,
  parseTargetFiles,
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
import GHC.Utils.TmpFs (TempDir (..))
import Internal.Args (Args (..))
import Internal.Cache (BinPath (..), Cache (..), CacheFeatures (..), ModuleArtifacts, Target (..), withCache)
import Internal.Error (handleExceptions)
import Internal.Log (Log (..), logToState)
import Prelude hiding (log)
import System.Environment (setEnv)

-- | Worker state.
data Env =
  Env {
    -- | Logger used to receive messages from GHC and relay them to Buck.
    log :: MVar Log,

    -- | Parts of @HscEnv@ we share between sessions.
    cache :: MVar Cache,

    -- | Preprocessed command line args from Buck.
    args :: Args
  }

-- | Add all the directories passed by Buck in @--bin-path@ options to the global @$PATH@.
-- Although Buck intends these to be module specific, all subsequent compile jobs will see all previous jobs' entries,
-- since we only have one process environment.
setupPath :: Args -> Cache -> IO Cache
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

dummyLocation :: a -> Located a
dummyLocation = mkGeneralLocated "by Buck2"

-- | Parse command line flags into @DynFlags@ and set up the logger. Extracted from GHC.
-- Returns the subset of args that have not been recognized as options.
parseFlags :: [Located String] -> Ghc (DynFlags, Logger, [Located String], DriverMessages)
parseFlags argv = do
  dflags0 <- GHC.getSessionDynFlags
  let dflags1 = dflags0 {ghcMode = OneShot, ghcLink = LinkBinary, verbosity = 0}
  logger1 <- getLogger
  let logger2 = setLogFlags logger1 (initLogFlags dflags1)
  (dflags, fileish_args, dynamicFlagWarnings) <- parseDynamicFlags logger2 dflags1 argv
  pure (dflags, setLogFlags logger2 (initLogFlags dflags), fileish_args, dynamicFlagWarnings)

-- | Parse CLI args and set up the GHC session.
-- Returns the subset of args that have not been recognized as options.
initGhc ::
  DynFlags ->
  Logger ->
  [Located String] ->
  DriverMessages ->
  Ghc [(String, Maybe Phase)]
initGhc dflags0 logger fileish_args dynamicFlagWarnings = do
  liftIO $ printOrThrowDiagnostics logger (initPrintConfig dflags0) (initDiagOpts dflags0) flagWarnings'
  let (dflags1, srcs, objs) = parseTargetFiles dflags0 (map unLoc fileish_args)
  unless (null objs) $ throwGhcException (UsageError "Targets contain object files")
  setSessionDynFlags dflags1
  pure srcs
  where
    flagWarnings' = GhcDriverMessage <$> dynamicFlagWarnings

-- | Run a program with a fresh session constructed from command line args.
-- Passes the unprocessed args to the callback, which usually consist of the file or module names intended for
-- compilation.
-- In a Buck compile step these should always be a single path, but in the metadata step they enumerate an entire unit.
withGhcInSession :: Env -> ([(String, Maybe Phase)] -> Ghc a) -> [Located String] -> Ghc a
withGhcInSession env prog argv = do
  pushLogHookM (const (logToState env.log))
  (dflags0, logger, fileish_args, dynamicFlagWarnings) <- parseFlags argv
  prettyPrintGhcErrors logger do
    srcs <- initGhc dflags0 logger fileish_args dynamicFlagWarnings
    prog srcs

-- | Create a base session and store it in the cache.
-- On subsequent calls, return the cached session, unless the cache is disabled or @reuse@ is true.
-- This will at some point be replaced by more deliberate methods.
ensureSession :: MVar Cache -> Args -> IO HscEnv
ensureSession cacheVar args =
  modifyMVar cacheVar \ cache -> do
    newEnv <- maybe (initHscEnv args.topdir) pure cache.baseSession
    if cache.features.enable
    then pure (cache {baseSession = Just newEnv}, newEnv)
    else pure (cache, newEnv)

-- | Run a @Ghc@ program to completion with a fresh clone of the base session.
-- See 'ensureSession' for @reuse@.
runSession :: Env -> ([Located String] -> Ghc (Maybe a)) -> IO (Maybe a)
runSession Env {log, args, cache} prog = do
  modifyMVar_ cache (setupPath args)
  hsc_env <- ensureSession cache args
  session <- Session <$> newIORef hsc_env
  flip unGhc session $ withSignalHandlers do
    traverse_ (modifySession . setTempDir) args.tempDir
    handleExceptions log Nothing (prog (map dummyLocation args.ghcOptions))

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
  runSession env $ withGhcInSession env \ srcs -> do
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
      result <- withCache env.log env.args.workerTargetId env.cache target do
        res <- prog
        pure do
          a <- res
          pure (Nothing, a)
      pure (snd <$> result)

-- | Run a @Ghc@ program to completion with a fresh clone of the base session augmented by some persisted state.
-- Return the interface and bytecode.
withGhcDefault :: Env -> (Target -> Ghc (Maybe (Maybe ModuleArtifacts, a))) -> IO (Maybe (Maybe ModuleArtifacts, a))
withGhcDefault env =
  withGhcUsingCache (withCache env.log env.args.workerTargetId env.cache) env
