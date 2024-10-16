module Logger (logHook) where

import GHC.Data.FastString (unpackFS)
import GHC.Driver.Flags (DumpFlag (Opt_D_dump_json))
import GHC.Types.Error (MessageClass (..), Severity (..), getCaretDiagnostic, mkLocMessageWarningGroups)
import GHC.Utils.Json (JsonDoc (..), ToJson (json), renderJSON)
import GHC.Utils.Logger
  ( LogAction,
    LogFlags (..),
    defaultLogActionHPrintDoc,
    defaultLogActionHPutStrDoc,
    log_dopt,
  )
import GHC.Utils.Outputable
  ( PprStyle (PprCode),
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
import GHC.Types.SrcLoc (SrcSpan (..), RealSrcSpan (..), srcSpanEndCol, srcSpanEndLine, srcSpanStartCol, srcSpanStartLine)
import System.IO (Handle)

--
-- override defaultLogAction. Redirect stdout/stderr to accumulated bytestring
--
logHook :: (Handle, Handle) -> LogAction -> LogAction
logHook (nstdout, nstderr) _ logflags msg_class srcSpan msg
  | log_dopt Opt_D_dump_json logflags = jsonLogAction' logflags msg_class srcSpan msg
  | otherwise = case msg_class of
      MCOutput                     -> -- putStrLn "AHAHAHAHA" >>
                                      printOut msg
      MCDump                       -> printOut (msg $$ blankLine)
      MCInteractive                -> putStrSDoc msg
      MCInfo                       -> printErrs msg
      MCFatal                      -> printErrs msg
      MCDiagnostic SevIgnore _ _   -> pure () -- suppress the message
      MCDiagnostic _sev _rea _code -> printDiagnostics
  where
    printOut   = defaultLogActionHPrintDoc  logflags False nstdout
    printErrs  = defaultLogActionHPrintDoc  logflags False nstderr
    putStrSDoc = defaultLogActionHPutStrDoc logflags False nstdout
    -- Pretty print the warning flag, if any (#10752)
    message = mkLocMessageWarningGroups (log_show_warn_groups logflags) msg_class srcSpan msg

    printDiagnostics = do
      caretDiagnostic <-
          if log_show_caret logflags
          then getCaretDiagnostic msg_class srcSpan
          else pure empty
      printErrs $ getPprStyle $ \style ->
        withPprStyle (setStyleColoured True style)
          (message $+$ caretDiagnostic $+$ blankLine)
      -- careful (#2302): printErrs prints in UTF-8,
      -- whereas converting to string first and using
      -- hPutStr would just emit the low 8 bits of
      -- each unicode char.

    jsonLogAction' :: LogAction
    jsonLogAction' _ (MCDiagnostic SevIgnore _ _) _ _ = return () -- suppress the message
    jsonLogAction' logflags msg_class srcSpan msg
      =
        defaultLogActionHPutStrDoc logflags True nstdout
          (withPprStyle PprCode (doc $$ text ""))
        where
          str = renderWithContext (log_default_user_context logflags) msg
          doc = renderJSON $
                  JSObject [ ( "span", spanToDumpJSON srcSpan )
                           , ( "doc" , JSString str )
                           , ( "messageClass", json msg_class )
                           ]
          spanToDumpJSON :: SrcSpan -> JsonDoc
          spanToDumpJSON s = case s of
                     (RealSrcSpan rss _) -> JSObject [ ("file", json file)
                                                    , ("startLine", json $ srcSpanStartLine rss)
                                                    , ("startCol", json $ srcSpanStartCol rss)
                                                    , ("endLine", json $ srcSpanEndLine rss)
                                                    , ("endCol", json $ srcSpanEndCol rss)
                                                    ]
                       where file = unpackFS $ srcSpanFile rss
                     UnhelpfulSpan _ -> JSNull
