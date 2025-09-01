module Internal.Log where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC (Ghc, Severity (SevIgnore), noSrcSpan)
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
import Types.Log (Log (..), LogLevel (..), Logger (..), TraceId (..))
import Types.Target (TargetSpec (..), renderTargetSpec)

-- | After the current request's target has been determined, the log state can be updated to generate more specific log
-- file paths.
setLogTarget :: MVar Log -> TargetSpec -> IO ()
setLogTarget logVar target =
  modifyMVar_ logVar \ log -> pure log {target = Just target}

newLogger :: MVar Log -> Logger
newLogger state =
  logger
  where
    debug = logOther logger LogDebug

    logger =
      Logger {
        withLog = modifyMVar state,
        setTarget = setLogTarget state,
        debug,
        debugD = debug . showPprUnsafe
      }


modifyLog :: Logger -> (Log -> IO Log) -> IO ()
modifyLog Logger {withLog} f =
  withLog \ l -> do
    new <- f l
    pure (new, ())

mapLog :: Logger -> (Log -> Log) -> IO ()
mapLog Logger {withLog} f =
  withLog \ l -> pure (f l, ())

withLog_ :: Logger -> (Log -> IO a) -> IO a
withLog_ Logger {withLog} f =
  withLog \ l -> do
    res <- f l
    pure (l, res)

logDiagnostics ::
  MonadIO m =>
  Logger ->
  String ->
  m ()
logDiagnostics logger msg =
  liftIO $ mapLog logger \ Log {diagnostics, ..} ->
    Log {diagnostics = msg : diagnostics, ..}

logOther ::
  MonadIO m =>
  Logger ->
  LogLevel ->
  String ->
  m ()
logOther logger level msg =
  liftIO $ mapLog logger \ Log {other, ..} ->
    Log {other = (msg, level) : other, ..}

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
    path = targetIdDir </> addExtension logName "log"

    targetIdDir | Just (TraceId wtId) <- traceId = logDir </> wtId
                | otherwise = logDir

    warn err = dbg ("Failed to write log file for " ++ logName ++ ": " ++ show err)

    logName = maybe "global" renderTargetSpec target

-- | Write the current session's log to a file, clear the fields in the 'MVar' and return the log lines.
logFlush :: Logger -> IO [String]
logFlush logger = do
  withLog logger \ Log {..} -> do
    let logLines = reverse (other ++ [(msg, LogInfo) | msg <- diagnostics])
    writeLogFile traceId target logLines
    pure (Log {diagnostics = [], other = [], ..}, [msg | (msg, level) <- logLines, LogInfo == level])

logToState :: Logger -> LogAction
logToState logger logflags msg_class srcSpan msg = case msg_class of
  MCOutput -> other msg
  MCDump -> other (msg $$ blankLine)
  MCInteractive -> other msg
  MCInfo -> diagnostic msg
  MCFatal -> diagnostic msg
  MCDiagnostic SevIgnore _ _ -> pure ()
  MCDiagnostic _sev _rea _code -> printDiagnostics
  where
    message = mkLocMessageWarningGroups (log_show_warn_groups logflags) msg_class srcSpan msg

    printDiagnostics = do
      caretDiagnostic <-
        if log_show_caret logflags
        then getCaretDiagnostic msg_class srcSpan
        else pure empty
      diagnostic $ getPprStyle $ \style ->
        withPprStyle (setStyleColoured True style) (message $+$ caretDiagnostic $+$ blankLine)

    diagnostic = logDiagnostics logger . render

    other = logOther logger LogInfo . render

    render d = renderWithContext (log_default_user_context logflags) d

dbg :: MonadIO m => String -> m ()
dbg = liftIO . hPutStrLn stderr

dbgs :: Show a => MonadIO m => a -> m ()
dbgs = dbg . show

dbgp :: Outputable a => MonadIO m => a -> m ()
dbgp = dbg . showPprUnsafe

logp ::
  Outputable a =>
  MonadIO m =>
  Logger ->
  a ->
  m ()
logp logger =
  liftIO . logOther logger LogInfo . showPprUnsafe

logd ::
  MonadIO m =>
  Logger ->
  SDoc ->
  m ()
logd = logp

logDebug ::
  MonadIO m =>
  Logger ->
  String ->
  m ()
logDebug logger =
  liftIO . logger.debug

logDebugP ::
  Outputable a =>
  MonadIO m =>
  Logger ->
  a ->
  m ()
logDebugP logger =
  liftIO . logOther logger LogDebug . showPprUnsafe

logDebugD ::
  MonadIO m =>
  Logger ->
  SDoc ->
  m ()
logDebugD =
  logDebugP

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
