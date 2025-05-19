module Internal.Log where

import Data.ByteString (ByteString)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Control.Concurrent.MVar (MVar, modifyMVar_, modifyMVar, newMVar)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
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
import System.IO (hPutStrLn, stderr)

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

logFlush :: MVar Log -> IO [String]
logFlush var =
  modifyMVar var \ Log {..} -> pure (Log {diagnostics = [], other = [], debug}, reverse (other ++ diagnostics))

logFlushBytes :: MVar Log -> IO ByteString
logFlushBytes var = do
  lns <- logFlush var
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
{-
  -- liftIO $
  --   logOutput (hsc_logger hsc_env) (pprHsBytes "IWKIM Here")
  let dflags = hsc_dflags hsc_env
      diagOpts = initDiagOpts dflags
      reason = WarningWithoutFlag
  let msg =
        DriverUnknownMessage $
          mkSimpleUnknownDiagnostic $
          mkPlainDiagnostic reason noHints $
          text "HELLO THERE"

  let warns = -- emptyMessages
        singleMessage (mkPlainMsgEnvelope diagOpts noSrcSpan msg)
  -- liftIO $ printOrThrowDiagnostics (hsc_logger hsc_env) (initPrintConfig dflags) (initDiagOpts dflags) (GhcDriverMessage <$> warns)
-}
