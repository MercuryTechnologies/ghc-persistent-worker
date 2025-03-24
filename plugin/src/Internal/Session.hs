module Internal.Session where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_)
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

setTempDir :: String -> HscEnv -> HscEnv
setTempDir dir = hscUpdateFlags \ dflags -> dflags {tmpDir = TempDir dir}

dummyLocation :: a -> Located a
dummyLocation = mkGeneralLocated "by Buck2"

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

withGhcInSession :: Env -> ([(String, Maybe Phase)] -> Ghc a) -> [Located String] -> Ghc a
withGhcInSession env prog argv = do
  pushLogHookM (const (logToState env.log))
  (dflags0, logger, fileish_args, dynamicFlagWarnings) <- parseFlags argv
  prettyPrintGhcErrors logger do
    srcs <- initGhc dflags0 logger fileish_args dynamicFlagWarnings
    prog srcs

ensureSession :: Bool -> MVar Cache -> Args -> IO HscEnv
ensureSession reuse cacheVar args =
  modifyMVar cacheVar \ cache -> do
    if cache.features.enable && reuse
    then do
      newEnv <- maybe (initHscEnv args.topdir) pure cache.baseSession
      pure (cache {baseSession = Just newEnv}, newEnv)
    else do
      newEnv <- initHscEnv args.topdir
      pure (cache, newEnv)

runSession :: Bool -> Env -> ([Located String] -> Ghc (Maybe a)) -> IO (Maybe a)
runSession reuse Env {log, args, cache} prog = do
  modifyMVar_ cache (setupPath args)
  hsc_env <- ensureSession reuse cache args
  session <- Session <$> newIORef hsc_env
  flip unGhc session $ withSignalHandlers do
    traverse_ (modifySession . setTempDir) args.tempDir
    handleExceptions log Nothing (prog (map dummyLocation args.ghcOptions))

ensureSingleTarget :: [(String, Maybe Phase)] -> Ghc Target
ensureSingleTarget = \case
  [(src, Nothing)] -> pure (Target src)
  [(_, phase)] -> panic ("Called worker with unexpected start phase: " ++ show phase)
  args -> panic ("Called worker with multiple targets: " ++ show args)

withGhcUsingCache :: (Target -> Ghc a -> Ghc (Maybe b)) -> Env -> (Target -> Ghc a) -> IO (Maybe b)
withGhcUsingCache cacheHandler env prog =
  runSession True env $ withGhcInSession env \ srcs -> do
    target <- ensureSingleTarget srcs
    cacheHandler target do
      initializeSessionPlugins
      prog target

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

withGhcDefault :: Env -> (Target -> Ghc (Maybe (Maybe ModuleArtifacts, a))) -> IO (Maybe (Maybe ModuleArtifacts, a))
withGhcDefault env =
  withGhcUsingCache (withCache env.log env.args.workerTargetId env.cache) env

specificPrefixSwitches :: [String]
specificPrefixSwitches =
  [
    "-i"
  ]

specificSwitches :: [String]
specificSwitches =
  [
    "-o",
    "-dyno",
    "-ohi",
    "-dynohi",
    "-this-unit-id",
    "-package",
    "-package-id",
    "-stubdir"
  ]

isSpecificPrefix :: String -> Bool
isSpecificPrefix arg =
  any (`isPrefixOf` arg) specificPrefixSwitches

isSpecific :: String -> Bool
isSpecific =
  flip elem specificSwitches

withGhcInSessionGeneral ::
  Env ->
  ([String] -> [(String, Maybe Phase)] -> Ghc (Maybe a)) ->
  IO (Maybe a)
withGhcInSessionGeneral env prog =
  runSession True env1 $ withGhcInSession env1 (prog specific)
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

withGhcUsingCacheGeneral ::
  (Target -> Ghc a -> Ghc (Maybe b)) ->
  Env ->
  ([String] -> Target -> Ghc a) ->
  IO (Maybe b)
withGhcUsingCacheGeneral cacheHandler env prog =
  withGhcInSessionGeneral env \ specific srcs -> do
    target <- ensureSingleTarget srcs
    cacheHandler target do
      initializeSessionPlugins
      prog specific target

withGhcGeneral :: Env -> ([String] -> Target -> Ghc (Maybe a)) -> IO (Maybe a)
withGhcGeneral env =
  withGhcUsingCacheGeneral cacheHandler env
  where
    cacheHandler target prog = do
      result <- withCache env.log env.args.workerTargetId env.cache target do
        res <- prog
        pure do
          a <- res
          pure (Nothing, a)
      pure (snd <$> result)

withGhcGeneralDefault ::
  Env ->
  ([String] -> Target -> Ghc (Maybe (Maybe ModuleArtifacts, a))) ->
  IO (Maybe (Maybe ModuleArtifacts, a))
withGhcGeneralDefault env =
  withGhcUsingCacheGeneral (withCache env.log env.args.workerTargetId env.cache) env
