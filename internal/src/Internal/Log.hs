module Internal.Log where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, onException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Fixed (Milli, Pico)
import Data.Foldable (traverse_)
import Data.Hashable (hash)
import Data.Time (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import GHC (Ghc, Severity (SevIgnore), SrcSpan, noSrcSpan)
import GHC.Driver.Config.Diagnostic (initDiagOpts)
import GHC.Driver.DynFlags (getDynFlags)
import GHC.Driver.Errors.Types (DriverMessage (..), GhcMessage (GhcDriverMessage))
import GHC.Driver.Monad qualified as GHC (logDiagnostics)
import GHC.Types.Error (
  DiagnosticReason (WarningWithoutFlag),
  MessageClass (..),
  getCaretDiagnostic,
  mkLocMessageWarningGroups,
  mkPlainDiagnostic,
  mkSimpleUnknownDiagnostic,
  noHints,
  singleMessage,
  )
import GHC.Utils.Error (mkPlainMsgEnvelope)
import GHC.Utils.Logger (LogAction, LogFlags (..))
import GHC.Utils.Outputable (
  Outputable,
  SDoc,
  blankLine,
  empty,
  getPprStyle,
  renderWithContext,
  setStyleColoured,
  showPprUnsafe,
  withPprStyle,
  ($$),
  ($+$),
  )
import Prelude hiding (log)
import System.Directory (createDirectoryIfMissing, doesPathExist)
import System.FilePath (addExtension, takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (tryIOError)
import Text.Printf (printf)
import Types.Log (Log (..), LogLevel (..), Logger (..), TraceId (..))
import Types.Target (TargetSpec (..), renderTargetSpec)

-- | After the current request's target has been determined, the log state can be updated to generate more specific log
-- file paths.
setLogTarget :: MVar Log -> TargetSpec -> IO ()
setLogTarget logVar target =
  modifyMVar_ logVar \ log -> pure log {target = Just target}

mapLog :: MVar Log -> (Log -> Log) -> IO ()
mapLog logVar f =
  modifyMVar logVar \ l -> pure (f l, ())

withLog_ :: MVar Log -> (Log -> IO a) -> IO a
withLog_ logVar f =
  modifyMVar logVar \ l -> do
    res <- f l
    pure (l, res)

logDiagnostics ::
  MonadIO m =>
  MVar Log ->
  String ->
  m ()
logDiagnostics logVar msg =
  liftIO $ mapLog logVar \ Log {diagnostics, ..} ->
    Log {diagnostics = msg : diagnostics, ..}

logOther ::
  MonadIO m =>
  MVar Log ->
  LogLevel ->
  String ->
  m ()
logOther logVar level msg =
  liftIO $ mapLog logVar \ Log {other, ..} ->
    Log {other = (msg, level) : other, ..}

decorateDiagnostic ::
  LogFlags ->
  MessageClass ->
  SrcSpan ->
  SDoc ->
  IO SDoc
decorateDiagnostic flags msg_class srcSpan msg = do
  caretDiagnostic <-
    if log_show_caret flags
    then getCaretDiagnostic msg_class srcSpan
    else pure empty
  pure $ getPprStyle \ style ->
    withPprStyle (setStyleColoured True style) (message $+$ caretDiagnostic $+$ blankLine)
  where
    message = mkLocMessageWarningGroups (log_show_warn_groups flags) msg_class srcSpan msg

renderLogMessage :: LogFlags -> SDoc -> String
renderLogMessage flags = renderWithContext (log_default_user_context flags)

-- | This mostly resembles the native GHC action, but we write everything to the 'Log' state.
logGhcAction :: MVar Log -> LogAction
logGhcAction logVar flags msg_class srcSpan msg = case msg_class of
  MCOutput -> other msg
  MCDump -> other (msg $$ blankLine)
  MCInteractive -> other msg
  MCInfo -> diagnostic msg
  MCFatal -> diagnostic msg
  MCDiagnostic SevIgnore _ _ -> pure ()
  MCDiagnostic _sev _rea _code -> diagnostic =<< decorateDiagnostic flags msg_class srcSpan msg
  where
    diagnostic = logDiagnostics logVar . renderLogMessage flags

    other = logOther logVar LogInfo . renderLogMessage flags

logDir :: FilePath
logDir =
  "/tmp/ghc-persistent-worker/log"

-- | Write the current session's log to a file in 'logDir', using the provided 'LogName' as relative path.
--
-- This name is usually the name of the module being compiled, or @metadata@.
--
-- If the session fails before the target could be determined, this is 'Nothing', so we choose @unknown@ for the file
-- name.
writeLogFile :: Maybe TraceId -> Maybe TargetSpec -> [(String, LogLevel)] -> IO ()
writeLogFile traceId target logLines =
  either warn pure =<< tryIOError do
    createDirectoryIfMissing True (takeDirectory path)
    exists <- doesPathExist path
    unless exists do
      writeFile path ""
    appendFile path (unlines (fst <$> logLines))
  where
    path = targetIdDir </> addExtension logName' "log"

    targetIdDir | Just (TraceId wtId) <- traceId = logDir </> wtId
                | otherwise = logDir

    warn err = dbg ("Failed to write log file for " ++ logName ++ ": " ++ show err)

    logName = maybe "global" renderTargetSpec target
    logName'
      | length logName > 250 = take 220 logName ++ "_" ++ printf "%.16x" (hash logName)
      | otherwise = logName

logFlushWith :: (Log -> [(String, LogLevel)] -> IO a) -> MVar Log -> IO a
logFlushWith use logVar =
  modifyMVar logVar \ log@Log {other, diagnostics} -> do
    let logLines = reverse (other ++ [(msg, LogInfo) | msg <- diagnostics])
    result <- use log logLines
    pure (log {diagnostics = [], other = []}, result)

-- | Write the current session's log to a file, clear the fields in the 'MVar' and return the log lines.
logFlush :: MVar Log -> IO [String]
logFlush =
  logFlushWith \ Log {traceId, target} logLines -> do
    writeLogFile traceId target logLines
    pure [msg | (msg, level) <- logLines, LogInfo == level]

-- | Write the current session's log to stderr and clear the fields in the 'MVar'.
logFlushDebug :: MVar Log -> IO ()
logFlushDebug =
  logFlushWith (const (traverse_ (dbg . fst)))

newLogger :: MVar Log -> Logger
newLogger state =
  logger
  where
    debug = logOther state LogDebug

    info = logOther state LogInfo

    logger =
      Logger {
        setTarget = setLogTarget state,
        debug,
        debugD = debug . showPprUnsafe,
        info,
        infoD = info . showPprUnsafe,
        fatal = info . showPprUnsafe,
        ghcAction = logGhcAction state,
        flush = logFlush state
      }

dbg :: MonadIO m => String -> m ()
dbg = liftIO . hPutStrLn stderr

dbgs :: Show a => MonadIO m => a -> m ()
dbgs = dbg . show

dbgp :: Outputable a => MonadIO m => a -> m ()
dbgp = dbg . showPprUnsafe

logDebugD ::
  MonadIO m =>
  Logger ->
  SDoc ->
  m ()
logDebugD log =
  liftIO . log.debugD

ghcLogd :: SDoc -> Ghc ()
ghcLogd doc = do
  dflags <- getDynFlags
  let diagOpts = initDiagOpts dflags
      reason = WarningWithoutFlag
      msg =
        DriverUnknownMessage $
          mkSimpleUnknownDiagnostic $
          mkPlainDiagnostic reason noHints $
          doc
      msgs = singleMessage (mkPlainMsgEnvelope diagOpts noSrcSpan msg)
  GHC.logDiagnostics (GhcDriverMessage <$> msgs)

-- | Run the given computation and write the given description and the elapsed real time it took to the debug log.
logTimed ::
  MonadIO m =>
  MonadCatch m =>
  Logger ->
  String ->
  m a ->
  m a
logTimed logger desc ma = do
  start <- liftIO getCurrentTime
  res <- onException ma do
    liftIO $ logger.debug ("Timed computation failed: " ++ desc)
  liftIO do
    end <- getCurrentTime
    logger.debug (desc ++ " | " ++ show (realToFrac @Pico @Milli (nominalDiffTimeToSeconds (diffUTCTime end start))))
    pure res

-- | Like 'logTimed', but takes an 'SDoc'.
logTimedD ::
  MonadIO m =>
  MonadCatch m =>
  Logger ->
  SDoc ->
  m a ->
  m a
logTimedD logger =
  logTimed logger . showPprUnsafe
