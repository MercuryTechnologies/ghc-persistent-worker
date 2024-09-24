{-# language BlockArguments, DerivingStrategies #-}

module Log where

import Control.Concurrent.MVar (MVar, modifyMVar_)
import GHC (Severity (SevIgnore))
import GHC.Types.Error (MessageClass (..), getCaretDiagnostic, mkLocMessageWarningGroups)
import GHC.Utils.Logger (LogAction, LogFlags (..))
import GHC.Utils.Outputable (
  blankLine,
  empty,
  getPprStyle,
  renderWithContext,
  setStyleColoured,
  text,
  withPprStyle,
  ($$),
  ($+$),
  )
import Prelude hiding (log)

data Log =
  Log {
    diagnostics :: [String],
    other :: [String]
  }
  deriving stock (Eq, Show)

logToState :: MVar Log -> LogAction
logToState logVar logflags msg_class srcSpan msg = case msg_class of
  MCOutput -> logOther msg
  MCDump -> logOther (msg $$ blankLine)
  MCInteractive -> logOther msg
  MCInfo -> logError msg
  MCFatal -> logError msg
  MCDiagnostic SevIgnore _ _ -> pure ()
  MCDiagnostic _sev _rea _code -> printDiagnostics
  where
    message = mkLocMessageWarningGroups (log_show_warn_groups logflags) msg_class srcSpan msg

    printDiagnostics = do
      caretDiagnostic <-
        if log_show_caret logflags
        then getCaretDiagnostic msg_class srcSpan
        else pure empty
      logError $ getPprStyle $ \style ->
        withPprStyle (setStyleColoured True style) (message $+$ caretDiagnostic $+$ blankLine)

    logError =
      logWith \ Log {diagnostics, other} new ->
        Log {diagnostics = new : diagnostics, other}

    logOther =
      logWith \ Log {diagnostics, other} new ->
        Log {diagnostics = diagnostics, other = new : other}

    logWith f d =
      modifyMVar_ logVar \ log ->
        let new = renderWithContext (log_default_user_context logflags) (d $$ text "")
        in pure (f log new)
