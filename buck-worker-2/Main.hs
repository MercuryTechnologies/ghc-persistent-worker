{-# language DataKinds, OverloadedLists, OverloadedStrings, GADTs #-}

module Main where

import Internal.Log (dbg)
import Args (BuckArgs (..), CompileResult (..), parseBuckArgs, toGhcArgs, writeResult)
import BuckWorker (
  ExecuteCommand (..),
  ExecuteCommand_EnvironmentEntry (..),
  ExecuteEvent (..),
  ExecuteResponse (..),
  Worker (..),
  workerServer,
  )
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Exception (SomeException (SomeException), throwIO, try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Data.String (fromString)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import qualified Data.Text.Lazy as LazyText
import qualified Data.Vector as Vector
import GHC (getSession)
import Internal.AbiHash (readAbiHash)
import Internal.Args (Args (..))
import Internal.Cache (Cache (..), emptyCache)
import Internal.Log (newLog)
import Internal.Session (Env (..), withGhc)
import Message
import Network.GRPC.HighLevel.Generated (
  GRPCMethodType (..),
  ServerRequest (..),
  ServerResponse (..),
  ServiceOptions (..),
  StatusCode (..),
  defaultServiceOptions,
  )
import Pool (Pool (..), dumpStatus, removeWorker)
import Prelude hiding (log)
import Server (assignLoop)
import System.Environment (lookupEnv)
import System.IO (BufferMode (LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)
import Worker (work)

commandEnv :: Vector.Vector ExecuteCommand_EnvironmentEntry -> Map String String
commandEnv =
  Map.fromList .
  fmap (\ (ExecuteCommand_EnvironmentEntry key value) -> (fromBs key, fromBs value)) .
  Vector.toList
  where
    fromBs = Text.unpack . decodeUtf8Lenient

abiHashIfSuccess :: Env -> BuckArgs -> Int -> IO (Maybe CompileResult)
abiHashIfSuccess env args code
  | 0 == code
  = withGhc env \ _ -> do
    hsc_env <- getSession
    abiHash <- readAbiHash hsc_env args.abiOut
    pure (Just CompileResult {abiHash})
  | otherwise
  = pure Nothing

note :: String -> Maybe a -> ExceptT String IO a
note msg = \case
  Just a -> pure a
  Nothing -> throwE msg

processRequest :: TVar Pool -> BuckArgs -> Env -> IO (Maybe CompileResult, String)
processRequest pool buckArgs env@Env {args} = do
  either (Nothing,) id <$> runExceptT do
    ghcPath <- note "no --ghc-path given" args.ghcPath
    requestWorkerTargetId <- Just . TargetId <$> note "no --worker-target-id given" args.workerTargetId
    liftIO do
      (j, i, hset) <- assignLoop ghcPath (maybeToList buckArgs.pluginDb) pool requestWorkerTargetId
      let
        req = Request {
          requestWorkerTargetId,
          requestWorkerClose = False,
          requestEnv = Map.toList args.env,
          requestArgs = "-c" : args.ghcOptions
          }
      Response {responseResult = code, ..} <- runReaderT (work req) (j, i, hset, pool)
      result <- abiHashIfSuccess env buckArgs code
      dbg ("Code: " ++ show code)
      dbg ("Result: " ++ show result)
      when (requestWorkerClose req) do
        traverse_ (removeWorker pool) requestWorkerTargetId
      dumpStatus pool
      pure (result, unlines (responseConsoleStdOut ++ responseConsoleStdErr))

executeHandler ::
  MVar Cache ->
  TVar Pool ->
  ServerRequest 'Normal ExecuteCommand ExecuteResponse ->
  IO (ServerResponse 'Normal ExecuteResponse)
executeHandler cache pool (ServerNormalRequest _ ExecuteCommand {executeCommandArgv, executeCommandEnv}) = do
  hPutStrLn stderr (unlines argv)
  response <- either exceptionResponse successResponse =<< try run
  pure (ServerNormalResponse response [] StatusOk "")
  where
    run = do
      buckArgs <- either (throwIO . userError) pure (parseBuckArgs (commandEnv executeCommandEnv) argv)
      args <- toGhcArgs buckArgs
      log <- newLog False
      result <- processRequest pool buckArgs Env {cache, args, log}
      pure (buckArgs, result)

    successResponse (buckArgs, (result, diagnostics)) = do
      executeResponseExitCode <- writeResult buckArgs result
      pure ExecuteResponse {
        executeResponseExitCode,
        executeResponseStderr = LazyText.pack diagnostics
      }

    exceptionResponse (SomeException e) =
      pure ExecuteResponse {
        executeResponseExitCode = 1,
        executeResponseStderr = "Uncaught exception: " <> LazyText.pack (show e)
      }

    argv = Text.unpack . decodeUtf8Lenient <$> Vector.toList executeCommandArgv

execHandler ::
  ServerRequest 'ClientStreaming ExecuteEvent ExecuteResponse ->
  IO (ServerResponse 'ClientStreaming ExecuteResponse)
execHandler (ServerReaderRequest _metadata _recv) = do
  hPutStrLn stderr "Received Exec"
  error "not implemented"

handlers :: MVar Cache -> TVar Pool -> Worker ServerRequest ServerResponse
handlers cache srv =
  Worker
    { workerExecute = executeHandler cache srv,
      workerExec = execHandler
    }

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  socket <- lookupEnv "WORKER_SOCKET"
  hPutStrLn stderr $ "using worker socket: " <> show socket
  let
    n = 1
    thePool = Pool
        { poolLimit = n,
          poolNewWorkerId = 1,
          poolNewJobId = 1,
          poolStatus = mempty,
          poolHandles = []
        }

  poolRef <- newTVarIO thePool
  cache <- emptyCache False
  workerServer (handlers cache poolRef) (maybe id setSocket socket defaultServiceOptions)
  where
    setSocket s options = options {serverHost = fromString ("unix://" <> s <> "\x00"), serverPort = 0}
