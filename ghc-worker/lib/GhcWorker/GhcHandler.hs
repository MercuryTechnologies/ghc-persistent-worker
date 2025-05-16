module GhcWorker.GhcHandler where

import Control.Concurrent (MVar, forkIO, threadDelay)
import Control.Exception (throwIO)
import Control.Monad.Catch (onException)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.Int (Int32)
import GHC (DynFlags (..), Ghc, getSession)
import GHC.Driver.DynFlags (GhcMode (..))
import GHC.Driver.Env (hscUpdateFlags)
import GHC.Driver.Monad (modifySession)
import GhcWorker.BuckArgs (CompileResult (..), writeCloseOutput, writeResult)
import GhcWorker.Grpc (GrpcHandler (..))
import GhcWorker.Instrumentation (Hooks (..), InstrumentedHandler (..))
import Internal.AbiHash (AbiHash (..), showAbiHash)
import Internal.Cache (Cache (..), ModuleArtifacts (..), Target (..))
import Internal.Compile (compileModuleWithDepsInEps)
import Internal.CompileHpt (compileModuleWithDepsInHpt)
import Internal.Log (LogName (..), dbg, logFlush, newLog)
import Internal.Metadata (computeMetadata)
import Internal.Session (Env (..), withGhc, withGhcMhu)
import Prelude hiding (log)
import System.Exit (exitSuccess)
import System.FilePath (takeBaseName)
-- import System.Posix.Process (exitImmediately)
import Types.BuckArgs (BuckArgs, Mode (..), parseBuckArgs, toGhcArgs)
import qualified Types.BuckArgs
import Types.GhcHandler (WorkerMode (..))

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
  WorkerMode ->
  Hooks ->
  Env ->
  BuckArgs ->
  IO (Int32, Maybe Target)
dispatch workerMode hooks env args =
  case args.mode of
    Just ModeCompile -> do
      result <- compile
      code <- writeResult args (fst <$> result)
      pure (code, snd <$> result)
    Just ModeMetadata -> do
      code <- computeMetadata env <&> \case
        True -> 0
        False -> 1
      pure (code, Just (Target "metadata"))
    Just ModeClose -> do
      dbg "in dispatch. Mode Close"
      writeCloseOutput args
      forkIO $ do
        threadDelay 1_000_000
        exitImmediately ExitSuccess
      pure 0
      -- exitSuccess
    Just ModeTerminate -> do
      dbg "in dispatc. Mode Terminate"
      exitSuccess
    Just m -> error ("worker: mode not implemented: " ++ show m)
    Nothing -> error "worker: no mode specified"
  where
    compile = case workerMode of
      WorkerOneshotMode ->
        withGhc env (withTarget (compileAndReadAbiHash OneShot compileModuleWithDepsInEps hooks args))
      WorkerMakeMode ->
        withGhcMhu env \ _ ->
          withTarget (compileAndReadAbiHash CompManager compileModuleWithDepsInHpt hooks args)

    withTarget f target =
      f target <&> fmap \ r -> (r, target)

-- | Default implementation of an 'InstrumentedHandler' using our custom persistent worker GHC mode, either using HPT or
-- EPS for local dependency lookup.
--
-- Parses the request args from Buck, creates a new 'Log' and 'Env', and passes all of that to 'dispatch'.
-- Afterwards, extracts the log messages that GHC wrote to 'Log' and calls the instrumentation hook 'compileFinish',
-- providing the log and exit code.
--
-- If an exception was thrown, the hook is called without data.
ghcHandler ::
  MVar Cache ->
  WorkerMode ->
  InstrumentedHandler
ghcHandler cache workerMode =
  InstrumentedHandler \ hooks -> GrpcHandler \ commandEnv argv -> do
    buckArgs <- either (throwIO . userError) pure (parseBuckArgs commandEnv argv)
    args <- toGhcArgs buckArgs
    log <- newLog True
    let env = Env {log, cache, args}
    onException
      do
        (result, target) <- dispatch workerMode hooks env buckArgs
        output <- logFlush (logName <$> target) env.log
        liftIO $ hooks.compileFinish (Just (target, output, result))
        pure (output, result)
      do
        liftIO $ hooks.compileFinish Nothing
    where
      logName (Target target) = LogName (takeBaseName target)
