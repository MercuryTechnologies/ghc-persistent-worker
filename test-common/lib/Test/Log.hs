module Test.Log where

import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import GHC (Severity (..))
import GHC.Types.Error (MessageClass (..))
import GHC.Utils.Logger (LogAction)
import GHC.Utils.Outputable (showPprUnsafe)
import Internal.Log (dbg, decorateDiagnostic, renderLogMessage)
import Prelude hiding (log)
import Test.Data.TestLog (DiagnosticEntry (..), TestLog (..))
import qualified Types.Log as Log
import Types.Log (Logger (Logger))

modifyLog :: IORef TestLog -> (TestLog -> TestLog) -> IO ()
modifyLog logVar f =
  atomicModifyIORef' logVar \ l -> (f l, ())

-- | Not removing data from the state since we will only use this for debugging and don't want it to influence any
-- assertions.
testLogFlush :: IORef TestLog -> IO [String]
testLogFlush logVar = do
  TestLog {..} <- readIORef logVar
  pure (reverse messages ++ [d.rendered | d <- diagnostics] ++ fatal)

-- | Only process messages we care about: diagnostics and fatal errors.
testGhcAction :: IORef TestLog -> LogAction
testGhcAction logRef flags msg_class srcSpan msg = case msg_class of
  MCOutput -> pure ()
  MCDump -> pure ()
  MCInteractive -> pure ()
  MCInfo -> pure ()
  MCFatal ->
    modifyLog logRef \ TestLog {..} ->
      TestLog {fatal = showPprUnsafe msg : fatal, ..}
  MCDiagnostic SevIgnore _ _ -> pure ()
  MCDiagnostic sev _rea code -> do
    rendered <- renderLogMessage flags <$> decorateDiagnostic flags msg_class srcSpan msg
    modifyLog logRef \ TestLog {..} ->
      TestLog {diagnostics = DiagnosticEntry {code, severity = sev, rendered} : diagnostics, ..}

testLogger :: IORef TestLog -> Logger
testLogger logVar =
  Logger {
    setTarget = \ target -> modifyLog logVar \ log -> log {target = Just target},
    debug,
    debugD = debug . showPprUnsafe,
    info = debug,
    infoD = debug . showPprUnsafe,
    fatal = \ message -> modifyLog logVar \ log -> log {fatal = showPprUnsafe message : log.fatal},
    ghcAction = testGhcAction logVar,
    flush = testLogFlush logVar
  }
  where
    debug message = do
      modifyLog logVar \ TestLog {..} ->
        TestLog {messages = message : messages, ..}

newTestLog :: IO (Logger, IORef TestLog)
newTestLog = do
  logVar <- newIORef TestLog {target = Nothing, diagnostics = [], fatal = [], messages = []}
  pure (testLogger logVar, logVar)

dumpTestLog :: String -> Logger -> IO ()
dumpTestLog desc logger = do
  dbg $ "Test log for " ++ desc ++ ":"
  dbg ""
  traverse_ dbg =<< logger.flush
  dbg ""

withTestLog ::
  Bool ->
  String ->
  ((Logger, IORef TestLog) -> IO a) ->
  IO a
withTestLog dump label action = do
  result@(log, _) <- newTestLog
  a <- action result
  when dump do
    dumpTestLog label log
  pure a
