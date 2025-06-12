module GhcWorker.GhcHandler where

import Common.Grpc (GrpcHandler (..))
import Control.Concurrent (MVar, forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVar, retry, writeTVar)
import Control.Exception (throwIO, try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.Int (Int32)
import GHC (DynFlags (..), Ghc, getSession)
import GHC.Driver.DynFlags (GhcMode (..))
import GHC.Driver.Env (hscUpdateFlags)
import GHC.Driver.Monad (modifySession)
import GhcWorker.CompileResult (CompileResult (..), writeCloseOutput, writeResult)
import GhcWorker.Instrumentation (Hooks (..), InstrumentedHandler (..))
import Internal.AbiHash (AbiHash (..), showAbiHash)
import Internal.Compile (compileModuleWithDepsInEps)
import Internal.CompileHpt (compileModuleWithDepsInHpt)
import Internal.Log (TraceId, dbg, logFlush, newLog, setLogTarget)
import Internal.Metadata (computeMetadata)
import Internal.Session (Env (..), withGhc, withGhcMhu)
import Internal.State (ModuleArtifacts (..), WorkerState (..), dumpState)
import Prelude hiding (log)
import System.Exit (ExitCode (ExitSuccess))
import System.Posix.Process (exitImmediately)
import Types.BuckArgs (BuckArgs, Mode (..), parseBuckArgs, toGhcArgs)
import qualified Types.BuckArgs
import Types.GhcHandler (WorkerMode (..))
import Types.State (Target (Target))

data LockState = LockStart | LockFreeze Int | LockThaw Int | LockEnd
  deriving stock (Eq, Show)

withLock :: Int -> TVar LockState -> IO a -> IO a
withLock maxLock lock action = do
  _ <- atomically do
    s <- readTVar lock
    case s of
      LockStart -> writeTVar lock (LockFreeze maxLock) >> pure (LockFreeze maxLock)
      LockFreeze _ -> retry
      LockThaw n | n > 0 -> writeTVar lock (LockFreeze (n - 1)) >> pure (LockFreeze (n - 1))
      LockThaw 0 -> writeTVar lock LockEnd >> pure LockEnd
      x -> pure x
  r <- action
  atomically $ modifyTVar' lock \case
    LockFreeze n -> LockThaw n
    x -> x
  pure r

-- | Compile a single module.
-- Depending on @mode@ this will either use the old EPS-based oneshot-style compilation logic or the HPT-based
-- make-style implementation.
compileAndReadAbiHash ::
  GhcMode ->
  (Target -> Ghc (Maybe ModuleArtifacts)) ->
  Hooks ->
  BuckArgs ->
  Target ->
  Ghc (Maybe CompileResult)
compileAndReadAbiHash ghcMode compile hooks args target = do
  liftIO $ hooks.compileStart (Just target)
  modifySession $ hscUpdateFlags \ d -> d {ghcMode}
  compile target >>= traverse \ artifacts -> do
    hsc_env <- getSession
    let
      abiHash :: Maybe AbiHash
      abiHash = do
        path <- args.abiOut
        Just AbiHash {path, hash = showAbiHash hsc_env artifacts.iface}
    pure CompileResult {artifacts, abiHash}

-- | Process a worker request based on the operational mode specified in the request arguments, either compiling a
-- single module for 'ModeCompile' (@-c@), or computing and writing the module graph to a JSON file for 'ModeMetadata'
-- (@-M@).
dispatch ::
  TVar LockState ->
  WorkerMode ->
  Hooks ->
  Env ->
  BuckArgs ->
  IO (Int32, Maybe Target)
dispatch lock workerMode hooks env args =
  case args.mode of
    Just ModeCompile -> do
      let maxLock = 10
      (code, result) <- do
        withLock maxLock lock do
          result <- compile
          code <- writeResult args (fst <$> result)
          pure (code, result)
      pure (code, snd <$> result)
    Just ModeMetadata -> do
      let target = Target "metadata"
      liftIO $ setLogTarget env.log target
      code <- computeMetadata env <&> \case
        True -> 0
        False -> 1
      pure (code, Just target)
    Just ModeClose -> do
      dbg "in dispatch. Mode Close"
      _ <- writeCloseOutput args
      _ <- forkIO $ do
        threadDelay 1_000_000
        exitImmediately ExitSuccess
      pure (0, Nothing)
    Just m -> error ("worker: mode not implemented: " ++ show m)
    Nothing -> error "worker: no mode specified"
  where
    compile = case workerMode of
      WorkerOneshotMode ->
        withGhc env (withTarget (compileAndReadAbiHash OneShot compileModuleWithDepsInEps hooks args))
      WorkerMakeMode ->
        withGhcMhu env \ _ ->
          withTarget (compileAndReadAbiHash CompManager compileModuleWithDepsInHpt hooks args)

    withTarget f target = do
      liftIO $ setLogTarget env.log target
      f target <&> fmap \ r -> (r, target)

processResult ::
  Hooks ->
  Env ->
  Either IOError (Int32, Maybe Target) ->
  IO ([String], Int32)
processResult hooks env result = do
  when (exitCode /= 0) do
    dumpState env.log env.state exception
  output <- logFlush env.log
  hooks.compileFinish (hookPayload output)
  pure (output, exitCode)
  where
    hookPayload output =
      if exitCode == 0
      then Just (target, output, exitCode)
      else Nothing

    ((exitCode, target), exception) = case result of
      Right out -> (out, Nothing)
      Left err -> ((1, Nothing), Just ("Exception: " ++ show err))

-- | Default implementation of an 'InstrumentedHandler' using our custom persistent worker GHC mode, either using HPT or
-- EPS for local dependency lookup.
--
-- Parses the request args from Buck, creates a new 'Log' and 'Env', and passes all of that to 'dispatch'.
-- Afterwards, extracts the log messages that GHC wrote to 'Log' and calls the instrumentation hook 'compileFinish',
-- providing the log and exit code.
--
-- If an exception was thrown, the hook is called without data.
ghcHandler ::
  -- | first req lock hack
  TVar LockState ->
  MVar WorkerState ->
  WorkerMode ->
  Maybe TraceId ->
  InstrumentedHandler
ghcHandler lock state workerMode traceId =
  InstrumentedHandler \ hooks -> GrpcHandler \ commandEnv argv -> do
    buckArgs <- either (throwIO . userError) pure (parseBuckArgs commandEnv argv)
    args <- toGhcArgs buckArgs
    log <- newLog traceId
    let env = Env {log, state, args}
    result <- try $ dispatch lock workerMode hooks env buckArgs
    processResult hooks env result
