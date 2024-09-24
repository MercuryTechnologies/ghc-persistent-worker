{-# language BlockArguments, OverloadedRecordDot #-}

module Session where

import Args (Args (..))
import Cache (CacheRef, withCache)
import Control.Concurrent.MVar (MVar)
import Control.Monad.IO.Class (liftIO)
import Data.List (dropWhileEnd)
import Error (handleExceptions)
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
  runGhc,
  setSessionDynFlags,
  )
import GHC.Driver.Config.Diagnostic (initDiagOpts, initPrintConfig)
import GHC.Driver.Config.Logger (initLogFlags)
import GHC.Driver.Errors (printOrThrowDiagnostics)
import GHC.Driver.Errors.Types (DriverMessages, GhcMessage (GhcDriverMessage))
import GHC.Runtime.Loader (initializeSessionPlugins)
import GHC.Types.SrcLoc (Located, mkGeneralLocated, unLoc)
import GHC.Types.Unique.Supply (initUniqSupply)
import GHC.Utils.Logger (Logger, getLogger, setLogFlags)
import Log (Log (..), logToState)
import Prelude hiding (log)

data Env =
  Env {
    log :: MVar Log,
    cache :: CacheRef,
    args :: Args
  }

runSession :: Args -> ([Located String] -> Ghc (Maybe a)) -> IO (Maybe a)
runSession args prog = do
  topdir <- readPath args.ghcDirFile
  packageDb <- readPath args.ghcDbFile
  let packageDbArg path = ["-package-db", path]
      argv = args.ghcOptions ++ foldMap packageDbArg packageDb
  runGhc topdir do
    handleExceptions Nothing (prog (map loc argv))
  where
    readPath = fmap (fmap (dropWhileEnd ('\n' ==))) . traverse readFile
    loc = mkGeneralLocated "by Buck2"

parseFlags :: [Located String] -> Ghc (DynFlags, Logger, [Located String], DriverMessages)
parseFlags argv = do
  dflags0 <- GHC.getSessionDynFlags
  let dflags1 = dflags0 {ghcMode = OneShot, ghcLink = LinkBinary, verbosity = 1}
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
  let (dflags1, srcs, _objs) = parseTargetFiles dflags0 (map unLoc fileish_args)
  setSessionDynFlags dflags1
  dflags <- getSessionDynFlags
  liftIO $ initUniqSupply (initialUnique dflags) (uniqueIncrement dflags)
  initializeSessionPlugins
  pure srcs
  where
    flagWarnings' = GhcDriverMessage <$> dynamicFlagWarnings

withGhc :: Env -> ([(String, Maybe Phase)] -> Ghc (Maybe a)) -> IO (Maybe a)
withGhc Env {log, cache, args} prog =
  runSession args \ argv -> do
    pushLogHookM (const (logToState log))
    (dflags0, logger, fileish_args, dynamicFlagWarnings) <- parseFlags argv
    prettyPrintGhcErrors logger do
      srcs <- initGhc dflags0 logger fileish_args dynamicFlagWarnings
      withCache cache do
        prog srcs
