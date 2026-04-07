-- | Build log capture for the standalone GHC server.
module GhcServer.Log where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import GHC (Severity (..))
import GHC.Types.Error (MessageClass (..))
import GHC.Utils.Logger (LogAction)
import GHC.Utils.Outputable (showPprUnsafe)
import Internal.Log (decorateDiagnostic, renderLogMessage)
import Prelude hiding (log)
import System.IO (hPutStrLn, stderr)
import Types.Log (Logger (..))

-- | Captured log output for diagnostics.
data BuildLog =
  BuildLog {
    diagnostics :: [String],
    errors :: [String]
  }

emptyBuildLog :: BuildLog
emptyBuildLog = BuildLog [] []

modifyLog :: IORef BuildLog -> (BuildLog -> BuildLog) -> IO ()
modifyLog ref f = atomicModifyIORef' ref \ l -> (f l, ())

-- | Flush the build log, returning captured diagnostics and errors.
flushBuildLog :: IORef BuildLog -> IO [String]
flushBuildLog ref = do
  l <- readIORef ref
  pure (l.diagnostics ++ l.errors)

buildGhcAction :: IORef BuildLog -> LogAction
buildGhcAction logRef flags msg_class srcSpan msg = case msg_class of
  MCOutput -> pure ()
  MCDump -> pure ()
  MCInteractive -> pure ()
  MCInfo -> pure ()
  MCFatal ->
    modifyLog logRef \ l -> l {errors = showPprUnsafe msg : l.errors}
  MCDiagnostic SevIgnore _ _ -> pure ()
  MCDiagnostic _sev _rea _code -> do
    rendered <- renderLogMessage flags <$> decorateDiagnostic flags msg_class srcSpan msg
    modifyLog logRef \ l -> l {diagnostics = rendered : l.diagnostics}

-- | Create a logger.
--
-- When @verbose@ is 'True', debug and info messages are printed to stderr
-- synchronously.  Diagnostics and errors are always captured for 'flush'.
newLogger :: Bool -> IO Logger
newLogger verbose = do
  logRef <- newIORef emptyBuildLog
  pure Logger {
    setTarget = \ _ -> pure (),
    debug,
    debugD = debug . showPprUnsafe,
    info = debug,
    infoD = debug . showPprUnsafe,
    fatal = \ message ->
      modifyLog logRef \ l -> l {errors = showPprUnsafe message : l.errors},
    ghcAction = buildGhcAction logRef,
    flush = flushBuildLog logRef
  }
  where
    debug message
      | verbose = hPutStrLn stderr message
      | otherwise = pure ()

-- | Create a fresh per-task logger that captures output, running the given action.
--
-- Uses non-verbose mode since per-task loggers are internal; the main build
-- logger handles user-visible output.
withBuildLog :: (Logger -> IO a) -> IO a
withBuildLog action =
  action =<< newLogger False
