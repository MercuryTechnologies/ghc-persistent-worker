module GhcHandler where

import qualified BuckArgs as BuckArgs
import BuckArgs (BuckArgs, CompileResult (..), Mode (..), parseBuckArgs, toGhcArgs, writeResult)
import Control.Concurrent (MVar)
import Control.Exception (throwIO)
import Control.Monad.Catch (onException)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.Int (Int32)
import GHC (DynFlags (..), Ghc, getSession)
import GHC.Driver.DynFlags (GhcMode (..))
import GHC.Driver.Env (hscUpdateFlags)
import GHC.Driver.Monad (modifySession)
import Grpc (GrpcHandler (..))
import Instrumentation (Hooks (..), InstrumentedHandler (..))
import Internal.AbiHash (AbiHash (..), showAbiHash)
import Internal.Cache (Cache (..), ModuleArtifacts (..), Target (..))
import Internal.Compile (compileModuleWithDepsInEps)
import Internal.CompileHpt (compileModuleWithDepsInHpt)
import Internal.Log (logFlush, newLog)
import Internal.Metadata (computeMetadata)
import Internal.Session (Env (..), withGhc, withGhcMhu)
import Prelude hiding (log)

-- | Selects the worker implementation.
data WorkerMode =
  WorkerMakeMode
  |
  WorkerOneshotMode
  deriving stock (Eq, Show)

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
  IO Int32
dispatch workerMode hooks env args =
  case args.mode of
    Just ModeCompile -> do
      result <- compile
      writeResult args result
    Just ModeMetadata ->
      computeMetadata env <&> \case
        True -> 0
        False -> 1
    Just m -> error ("worker: mode not implemented: " ++ show m)
    Nothing -> error "worker: no mode specified"
  where
    compile = case workerMode of
      WorkerOneshotMode ->
        withGhc env (compileAndReadAbiHash OneShot compileModuleWithDepsInEps hooks args)
      WorkerMakeMode ->
        withGhcMhu env \ specific ->
          compileAndReadAbiHash CompManager (compileModuleWithDepsInHpt specific) hooks args

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
        result <- dispatch workerMode hooks env buckArgs
        output <- logFlush env.log
        liftIO $ hooks.compileFinish (Just (output, result))
        pure (output, result)
      do
        liftIO $ hooks.compileFinish Nothing
