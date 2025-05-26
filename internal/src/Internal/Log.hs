module Internal.Log where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import GHC (Ghc, Severity (SevIgnore), noSrcSpan)
import GHC.Driver.Config.Diagnostic (initDiagOpts)
import GHC.Driver.DynFlags (getDynFlags)
import GHC.Driver.Errors.Types (DriverMessage (..), GhcMessage(GhcDriverMessage))
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
import System.Directory (createDirectoryIfMissing, doesPathExist)
import System.FilePath (takeDirectory, (</>), addExtension)
import System.IO (hPutStrLn, stderr)

-- | Name of the current session's target for the log file path.
newtype LogName =
  LogName { get :: String }
  deriving stock (Eq, Show)

data Log =
  Log {
    diagnostics :: [String],
    other :: [String],
    debug :: Bool
  }
  deriving stock (Eq, Show)

newLog :: MonadIO m => Bool -> m (MVar Log)
newLog debug =
  liftIO $ newMVar Log {diagnostics = [], other = [], debug}

logDiagnostics ::
  MonadIO m =>
  MVar Log ->
  String ->
  m ()
logDiagnostics logVar msg =
  liftIO $ modifyMVar_ logVar \ Log {diagnostics, ..} -> do
    when debug (dbg msg)
    pure Log {diagnostics = msg : diagnostics, ..}

logOther ::
  MonadIO m =>
  MVar Log ->
  String ->
  m ()
logOther logVar msg =
  liftIO $ modifyMVar_ logVar \ Log {other, ..} -> do
    when debug (dbg msg)
    pure Log {other = msg : other, ..}

logDir :: FilePath
logDir =
  "/tmp/ghc-persistent-worker/log"

-- | Write the current session's log to a file in 'logDir', using the provided 'LogName' as relative path.
--
-- This name is usually the name of the module being compiled, or @metadata@.
--
-- If the session fails before the target could be determined, this is 'Nothing', so we choose @unknown@ for the file
-- name.
writeLogFile :: Maybe LogName -> [String] -> IO ()
writeLogFile logName logLines = do
  createDirectoryIfMissing True (takeDirectory path)
  exists <- doesPathExist path
  unless exists do
    writeFile path ""
  appendFile path (unlines logLines)
  where
    path = logDir </> addExtension (maybe "unknown" coerce logName) "log"

-- | Write the current session's log to a file, clear the fields in the 'MVar' and return the log lines.
logFlush :: Maybe LogName -> MVar Log -> IO [String]
logFlush logName var = do
  logLines <- modifyMVar var \ Log {..} -> pure (Log {diagnostics = [], other = [], debug}, reverse (other ++ diagnostics))
  writeLogFile logName logLines
  pure logLines

logFlushBytes :: MVar Log -> IO ByteString
logFlushBytes var = do
  lns <- logFlush Nothing var
  pure (encodeUtf8 (pack (unlines lns)))

logToState :: MVar Log -> LogAction
logToState logVar logflags msg_class srcSpan msg = case msg_class of
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

    diagnostic = logDiagnostics logVar . render

    other = logOther logVar . render

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
  MVar Log ->
  a ->
  m ()
logp logVar =
  logOther logVar . showPprUnsafe

logd ::
  MonadIO m =>
  MVar Log ->
  SDoc ->
  m ()
logd = logp


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
