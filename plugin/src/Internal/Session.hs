module Internal.Session where

import Control.Concurrent.MVar (MVar, modifyMVar_)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
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
  setSession,
  setSessionDynFlags,
  withSignalHandlers,
  )
import GHC.Driver.Config.Diagnostic (initDiagOpts, initPrintConfig)
import GHC.Driver.Config.Logger (initLogFlags)
import GHC.Driver.Env (hscUpdateFlags)
import GHC.Driver.Errors (printOrThrowDiagnostics)
import GHC.Driver.Errors.Types (DriverMessages, GhcMessage (GhcDriverMessage))
import GHC.Driver.Main (initHscEnv)
import GHC.Driver.Monad (Session (Session), unGhc)
import GHC.Runtime.Loader (initializeSessionPlugins)
import GHC.Types.SrcLoc (Located, mkGeneralLocated, unLoc)
import GHC.Utils.Logger (Logger, getLogger, setLogFlags)
import GHC.Utils.Panic (GhcException (UsageError), panic, throwGhcException)
import GHC.Utils.TmpFs (TempDir (..))
import Internal.Args (Args (..))
import Internal.Cache (BinPath (..), Cache (..), withCache)
import Internal.Error (handleExceptions)
import Internal.Log (Log (..), logToState)
import Prelude hiding (log)
import System.Environment (setEnv)

data Env =
  Env {
    log :: MVar Log,
    cache :: MVar Cache,
    args :: Args
  }

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

runSession :: Env -> ([Located String] -> Ghc (Maybe a)) -> IO (Maybe a)
runSession Env {log, args, cache} prog = do
  modifyMVar_ cache (setupPath args)
  ref <- newIORef (panic "empty session")
  let session = Session ref
  flip unGhc session $ withSignalHandlers do
    setSession . maybe id setTempDir args.tempDir =<< liftIO (initHscEnv args.topdir)
    handleExceptions log Nothing (prog (map loc args.ghcOptions))
  where
    loc = mkGeneralLocated "by Buck2"
    setTempDir dir = hscUpdateFlags \ dflags -> dflags {tmpDir = TempDir dir}

parseFlags :: [Located String] -> Ghc (DynFlags, Logger, [Located String], DriverMessages)
parseFlags argv = do
  dflags0 <- GHC.getSessionDynFlags
  let dflags1 = dflags0 {ghcMode = OneShot, ghcLink = LinkBinary, verbosity = 0}
  logger1 <- getLogger
  let logger2 = setLogFlags logger1 (initLogFlags dflags1)
  (dflags, fileish_args, dynamicFlagWarnings) <- parseDynamicFlags logger2 dflags1 argv
  pure (dflags, setLogFlags logger2 (initLogFlags dflags), fileish_args, dynamicFlagWarnings)

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

withGhc :: Env -> ([(String, Maybe Phase)] -> Ghc (Maybe a)) -> IO (Maybe a)
withGhc env prog =
  runSession env \ argv -> do
    pushLogHookM (const (logToState env.log))
    (dflags0, logger, fileish_args, dynamicFlagWarnings) <- parseFlags argv
    prettyPrintGhcErrors logger do
      srcs <- initGhc dflags0 logger fileish_args dynamicFlagWarnings
      withCache env.log env.cache (fst <$> srcs) do
        initializeSessionPlugins
        prog srcs
