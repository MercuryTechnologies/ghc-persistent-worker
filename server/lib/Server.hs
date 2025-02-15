{-# LANGUAGE NumericUnderscores #-}

module Server where

import Control.Concurrent (forkFinally)
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import qualified Control.Exception as E
import Control.Monad (forever, void, when)
import Control.Monad.Extra (untilJustM)
import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.IntMap as IM
import Message (TargetId, Request (..), recvMsg, sendMsg, unwrapMsg, wrapMsg)
import Network.Socket
  ( Family (AF_UNIX),
    SockAddr (..),
    Socket,
    SocketType (Stream),
    accept,
    bind,
    close,
    gracefulClose,
    listen,
    socket,
    withSocketsDo,
  )
import Pool
  ( HandleSet (..),
    JobId (..),
    Pool (..),
    WorkerId,
    assignJob,
    dumpStatus,
    newWorkerId,
    removeWorker,
  )
import System.Process (CreateProcess (std_in, std_out), StdStream (CreatePipe), createProcess, proc)
import Worker (Mailbox (..), mailboxForWorker, work)

runServer :: FilePath -> (Socket -> IO a) -> IO a
runServer socketFile server = do
  putStrLn "Start serving"
  withSocketsDo $ E.bracket (open socketFile) close loop
  where
    open fp = E.bracketOnError (socket AF_UNIX Stream 0) close $ \sock -> do
      bind sock (SockAddrUnix fp)
      listen sock 1024
      return sock
    loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
      $ \(conn, _peer) -> void $
        forkFinally (server conn) (const $ gracefulClose conn 5000)

initWorker ::
  -- | If @True@, use the custom implementation of the session management/compilation pipeline.
  -- Otherwise, call @GHCPersistentWorkerPlugin.compileMain@ when dispatching a job.
  Bool ->
  FilePath ->
  [FilePath] ->
  WorkerId ->
  IO HandleSet
initWorker implCustom ghcPath dbPaths i = do
  putStrLn $ "worker " ++ show i ++ " is initialized"
  let db_options = concatMap (\db -> ["-package-db", db]) dbPaths
      ghc_options =
        db_options ++
          [ "-plugin-package",
            "ghc-persistent-worker-plugin",
            "--frontend",
            "GHCPersistentWorkerPlugin",
            "-ffrontend-opt",
            show i,
            "-ffrontend-opt",
            if implCustom then "custom" else "default"
          ]
      proc_setup =
        (proc ghcPath ghc_options)
          { std_in = CreatePipe,
            std_out = CreatePipe
          }
  (Just hstdin, Just hstdout, _, ph) <- createProcess proc_setup
  let hset = HandleSet
        { handleProcess = ph,
          handleArgIn = hstdin,
          handleMsgOut = hstdout,
          handleMailbox = Mailbox []
        }
  pure hset

spawnWorker ::
  -- | If @True@, use the custom implementation of the session management/compilation pipeline.
  -- Otherwise, call @GHCPersistentWorkerPlugin.compileMain@ when dispatching a job.
  Bool ->
  FilePath ->
  [FilePath] ->
  TVar Pool ->
  IO (WorkerId, HandleSet)
spawnWorker implCustom ghcPath dbPaths poolRef = do
  wid <- atomically $ newWorkerId poolRef
  hset <- initWorker implCustom ghcPath dbPaths wid
  atomically $ do
    pool <- readTVar poolRef
    let s = poolStatus pool
        s' = IM.insert wid (0, Nothing) s
        ihsets = poolHandles pool
        ihsets' = (wid, hset) : ihsets
    writeTVar poolRef (pool {poolStatus = s', poolHandles = ihsets'})
  mailboxForWorker poolRef wid (handleMsgOut hset)
  pure (wid, hset)

-- | The parameter @implCustom@ determines whether to use the custom session/pipeline implementation.
-- In 'serve', it's hardcoded to @False@ to keep the usual behavior.
assignLoop :: Bool -> FilePath -> [FilePath] -> TVar Pool -> Maybe TargetId -> IO (JobId, WorkerId, HandleSet)
assignLoop implCustom ghcPath dbPaths poolRef mid = untilJustM $ do
  eassigned <- atomically $ assignJob poolRef mid
  case eassigned of
    Left n -> do
      putStrLn $ "currently " ++ show n ++ " jobs are running. I am spawning a worker."
      _ <- spawnWorker implCustom ghcPath dbPaths poolRef
      pure Nothing
    Right (j, i, hset) -> do
      putStrLn $ "Job assigned with ID: " ++ show j
      pure (Just (j, i, hset))

serve :: FilePath -> [FilePath] -> TVar Pool -> Socket -> IO ()
serve ghcPath dbPaths poolRef s = do
  !msg <- recvMsg s
  let req :: Request = unwrapMsg msg
      mid = requestWorkerTargetId req
  (j, i, hset) <- assignLoop False ghcPath dbPaths poolRef mid
  res <- runReaderT (work req) (j, i, hset, poolRef)
  sendMsg s (wrapMsg res)
  when (requestWorkerClose req) $
    case mid of
      Nothing -> pure ()
      Just id' -> removeWorker poolRef id'
  dumpStatus poolRef
  serve ghcPath dbPaths poolRef s
