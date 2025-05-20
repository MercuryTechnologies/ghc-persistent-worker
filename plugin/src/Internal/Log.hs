module Internal.Log where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import GHC (Severity (SevIgnore))
import GHC.Types.Error (MessageClass (..), getCaretDiagnostic, mkLocMessageWarningGroups)
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
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)

enableLog :: Bool
enableLog = True

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
  when enableLog do
    liftIO $ modifyMVar_ logVar \ Log {diagnostics, ..} -> do
      when debug (dbg msg)
      pure Log {diagnostics = msg : diagnostics, ..}

logOther ::
  MonadIO m =>
  MVar Log ->
  String ->
  m ()
logOther logVar msg =
  when enableLog do
    liftIO $ modifyMVar_ logVar \ Log {other, ..} -> do
      when debug (dbg msg)
      pure Log {other = msg : other, ..}

logDir :: FilePath
logDir =
  "/tmp/ghc-persistent-worker/log"

writeLogFile :: Maybe LogName -> [String] -> IO ()
writeLogFile logName logLines = do
  createDirectoryIfMissing True (takeDirectory path)
  exists <- doesPathExist path
  unless exists do
    writeFile path ""
  appendFile path (unlines logLines)
  where
    path = logDir </> maybe "unknown" coerce logName

logFlush :: Maybe LogName -> MVar Log -> IO [String]
logFlush logName var = do
  logLines <- modifyMVar var \ Log {..} -> pure (Log {diagnostics = [], other = [], debug}, reverse (other ++ diagnostics))
  writeLogFile logName logLines
  pure logLines

logFlushBytes :: Maybe LogName -> MVar Log -> IO ByteString
logFlushBytes logName var = do
  lns <- logFlush logName var
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
