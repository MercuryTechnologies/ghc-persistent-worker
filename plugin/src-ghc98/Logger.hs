module Logger where

--
-- override defaultLogAction. Redirect stdout/stderr to accumulated bytestring
--
logHook :: (Handle, Handle) -> LogAction -> LogAction
logHook (nstdout, nstderr) _ =
  \logflags msg_class srcSpan msg ->
    if (log_dopt Opt_D_dump_json logflags)
    then jsonLogAction'
    else case msg_class of
      MCOutput                     -> printOut msg
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
