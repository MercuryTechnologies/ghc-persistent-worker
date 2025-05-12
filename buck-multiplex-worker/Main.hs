{-# language DataKinds, GADTs #-}

module Main where

import BuckWorker (Worker (..))
import Control.Concurrent (Chan, MVar, newMVar)
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import GHC (getSession)
import GhcWorker.BuckArgs (CompileResult (..), writeResult)
import GhcWorker.Grpc (GrpcHandler (GrpcHandler), ghcServerMethods)
import GhcWorker.Instrumentation (Hooks (..), InstrumentedHandler (..), WorkerStatus (..), toGrpcHandler)
import GhcWorker.Orchestration (CreateMethods (..), runLocalGhc)
import GhcWorker.Run (createInstrumentMethods)
import Internal.AbiHash (readAbiHash)
import Internal.Cache (Cache (..), ModuleArtifacts (..), emptyCache)
import Internal.Log (newLog)
import Internal.Session (Env (..), withGhc)
import Message (Request (..), Response (..), TargetId (..))
import Network.GRPC.Common.Protobuf (Proto)
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (Methods (..))
import Pool (Pool (..), dumpStatus, removeWorker)
import Prelude hiding (log)
import qualified Proto.Instrument as Instr
import Server (assignLoop)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)
import Types.Args (Args (..))
import qualified Types.BuckArgs
import Types.BuckArgs (BuckArgs, parseBuckArgs, toGhcArgs)
import Types.Orchestration (envServerSocket)
import Worker (work)

-- | Write the compiled module's ABI hash to the Buck output file.
abiHashIfSuccess :: Env -> BuckArgs -> Int -> IO (Maybe CompileResult)
abiHashIfSuccess env args code
  | 0 == code
  = withGhc env \ _ -> do
    hsc_env <- getSession
    readAbiHash hsc_env args.abiOut <&> fmap \ (iface, abiHash) ->
      CompileResult {artifacts = ModuleArtifacts {iface, bytecode = Nothing}, abiHash = Just abiHash}
  | otherwise
  = pure Nothing

note :: String -> Maybe a -> ExceptT String IO a
note msg = \case
  Just a -> pure a
  Nothing -> throwE msg

-- | Compile a module by relaying the Buck request to a worker process orchestrated by the multiplexer logic.
dispatch ::
  TVar Pool ->
  BuckArgs ->
  Env ->
  IO ([String], Int32)
dispatch pool buckArgs env@Env {args} = do
  either failure id <$> runExceptT do
    ghcPath <- note "no --ghc-path given" args.ghcPath
    requestWorkerTargetId <- Just . TargetId <$> note "no --worker-target-id given" args.workerTargetId
    liftIO do
      (j, i, hset) <-
        assignLoop buckArgs.multiplexerCustom ghcPath (maybeToList buckArgs.pluginDb) pool requestWorkerTargetId
      let
        req = Request {
          requestWorkerTargetId,
          requestWorkerClose = False,
          requestEnv = Map.toList args.env,
          requestArgs = "-c" : args.ghcOptions
          }
      Response {responseResult = code, ..} <- runReaderT (work req) (j, i, hset, pool)
      result <- abiHashIfSuccess env buckArgs code
      when (requestWorkerClose req) do
        traverse_ (removeWorker pool) requestWorkerTargetId
      dumpStatus pool
      exitCode <- writeResult buckArgs result
      pure (responseConsoleStdOut ++ responseConsoleStdErr, exitCode)
  where
    failure msg = ([msg], 1)

-- | Implementation of 'InstrumentedHandler' for the process multiplexer.
-- Mostly identical to the default handler, just calling the custom 'dispatch'.
--
-- Calls the instrumentation hook 'compileStart' with no data, since the source file is parsed in the subprocess.
-- As of now, this is a no-op.
ghcHandler ::
  TVar Pool ->
  MVar Cache ->
  InstrumentedHandler
ghcHandler pool cache =
  InstrumentedHandler \ hooks -> GrpcHandler \ commandEnv argv -> do
    buckArgs <- either (throwIO . userError) pure (parseBuckArgs commandEnv argv)
    args <- toGhcArgs buckArgs
    log <- newLog False
    let env = Env {log, cache, args}
    hooks.compileStart Nothing
    dispatch pool buckArgs env

-- | Grapesy server methods for the process multiplexer.
createGhcMethods ::
  TVar Pool ->
  MVar WorkerStatus ->
  MVar Cache ->
  Maybe (Chan (Proto Instr.Event)) ->
  IO (Methods IO (ProtobufMethodsOf Worker))
createGhcMethods pool status cache instrChan =
  pure (ghcServerMethods (toGrpcHandler (ghcHandler pool cache) status cache instrChan))

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  socket <- envServerSocket
  hPutStrLn stderr $ "using worker socket: " <> show socket
  poolRef <- newTVarIO Pool {
    poolLimit = 1,
    poolNewWorkerId = 1,
    poolNewJobId = 1,
    poolStatus = mempty,
    poolHandles = []
  }
  cache <- emptyCache False
  status <- newMVar WorkerStatus {active = 0}
  let
    methods = CreateMethods {
      createInstrumentation = createInstrumentMethods cache,
      createGhc = createGhcMethods poolRef status cache
    }
  runLocalGhc methods socket Nothing
