{-# language DataKinds, GADTs #-}

module Main where

import BuckArgs (BuckArgs (..), CompileResult (..), parseBuckArgs, toGhcArgs, writeResult)
import BuckWorker (
  ExecuteCommand (..),
  ExecuteCommand'EnvironmentEntry (..),
  ExecuteEvent (..),
  ExecuteResponse (..),
  Worker (..),
  )
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Exception (SomeException (SomeException), throwIO, try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
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
import Internal.Cache (Cache (..), ModuleArtifacts (..), emptyCache)
import Internal.Log (newLog)
import Internal.Session (Env (..), withGhc)
import Message
import Pool (Pool (..), dumpStatus, removeWorker)
import Prelude hiding (log)
import Server (assignLoop)
import System.Environment (lookupEnv)
import System.IO (BufferMode (LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)
import Worker (work)

commandEnv :: Vector.Vector ExecuteCommand'EnvironmentEntry -> Map String String
commandEnv = undefined
  -- THIS CODE IS NOT YET COMPATIBLE WITH grapesy yet.
  -- Map.fromList .
  -- fmap (\ (ExecuteCommand'EnvironmentEntry key value) -> (fromBs key, fromBs value)) .
  --   Vector.toList
  -- where
  --   fromBs = Text.unpack . decodeUtf8Lenient

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

processRequest :: TVar Pool -> BuckArgs -> Env -> IO (Maybe CompileResult, String)
processRequest pool buckArgs env@Env {args} = do
  either (Nothing,) id <$> runExceptT do
    ghcPath <- note "no --ghc-path given" args.ghcPath
    requestWorkerTargetId <- Just . TargetId <$> note "no --worker-target-id given" args.workerTargetId
    liftIO do
      (j, i, hset) <- assignLoop buckArgs.multiplexerCustom ghcPath (maybeToList buckArgs.pluginDb) pool requestWorkerTargetId
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
      pure (result, unlines (responseConsoleStdOut ++ responseConsoleStdErr))

main :: IO ()
main = pure ()

-- TODO: Revice the following code.
--
-- THE FOLLOWING CODE IS NOT COMPATIBLE WITH grapesy YET.
--

-- executeHandler ::
--   MVar Cache ->
--   TVar Pool ->
--   ServerRequest 'Normal ExecuteCommand ExecuteResponse ->
--   IO (ServerResponse 'Normal ExecuteResponse)
-- executeHandler cache pool (ServerNormalRequest _ ExecuteCommand {executeCommandArgv, executeCommandEnv}) = do
--   hPutStrLn stderr (unlines argv)
--   response <- either exceptionResponse successResponse =<< try run
--   pure (ServerNormalResponse response [] StatusOk "")
--   where
--     run = do
--       buckArgs <- either (throwIO . userError) pure (parseBuckArgs (commandEnv executeCommandEnv) argv)
--       args <- toGhcArgs buckArgs
--       log <- newLog False
--       result <- processRequest pool buckArgs Env {cache, args, log}
--       pure (buckArgs, result)

--     successResponse (buckArgs, (result, diagnostics)) = do
--       executeResponseExitCode <- writeResult buckArgs result
--       pure ExecuteResponse {
--         executeResponseExitCode,
--         executeResponseStderr = LazyText.pack diagnostics
--       }

--     exceptionResponse (SomeException e) =
--       pure ExecuteResponse {
--         executeResponseExitCode = 1,
--         executeResponseStderr = "Uncaught exception: " <> LazyText.pack (show e)
--       }

--     argv = Text.unpack . decodeUtf8Lenient <$> Vector.toList executeCommandArgv

-- execHandler ::
--   ServerRequest 'ClientStreaming ExecuteEvent ExecuteResponse ->
--   IO (ServerResponse 'ClientStreaming ExecuteResponse)
-- execHandler (ServerReaderRequest _metadata _recv) = do
--   hPutStrLn stderr "Received Exec"
--   error "not implemented"

-- handlers :: MVar Cache -> TVar Pool -> Worker ServerRequest ServerResponse
-- handlers cache srv =
--   Worker
--     { workerExecute = executeHandler cache srv,
--       workerExec = execHandler
--     }

-- main :: IO ()
-- main = do
--   hSetBuffering stdout LineBuffering
--   hSetBuffering stderr LineBuffering
--   socket <- lookupEnv "WORKER_SOCKET"
--   hPutStrLn stderr $ "using worker socket: " <> show socket
--   let
--     n = 1
--     thePool = Pool
--         { poolLimit = n,
--           poolNewWorkerId = 1,
--           poolNewJobId = 1,
--           poolStatus = mempty,
--           poolHandles = []
--         }

--   poolRef <- newTVarIO thePool
--   cache <- emptyCache False
--   workerServer (handlers cache poolRef) (maybe id setSocket socket defaultServiceOptions)
--   where
--     setSocket s options = options {serverHost = fromString ("unix://" <> s <> "\x00"), serverPort = 0}
