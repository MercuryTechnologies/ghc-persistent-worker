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
    JobStatus (..),
    Pool (..),
    WorkerId,
    assignJob,
    dumpStatus,
    newWorkerId,
    removeWorker,
  )
import System.Process (CreateProcess (std_in, std_out), StdStream (CreatePipe), createProcess, proc)
import Worker (work)

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

initWorker :: FilePath -> [FilePath] -> WorkerId -> IO HandleSet
initWorker ghcPath dbPaths i = do
  putStrLn $ "worker " ++ show i ++ " is initialized"
  let db_options = concatMap (\db -> ["-package-db", db]) dbPaths
      ghc_options =
        db_options ++
          [ "-plugin-package",
            "ghc-persistent-worker-plugin",
            "--frontend",
            "GHCPersistentWorkerPlugin",
            "-ffrontend-opt",
            show i
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
          handleMsgOut = hstdout
        }
  pure hset

spawnWorker :: FilePath -> [FilePath] -> TVar Pool -> IO (WorkerId, HandleSet)
spawnWorker ghcPath dbPaths ref = do
  i <- atomically $ newWorkerId ref
  hset <- initWorker ghcPath dbPaths i
  atomically $ do
    pool <- readTVar ref
    let s = poolStatus pool
        s' = IM.insert i (False, Nothing) s
        ihsets = poolHandles pool
        ihsets' = (i, hset) : ihsets
    writeTVar ref (pool {poolStatus = s', poolHandles = ihsets'})
  pure (i, hset)


assignLoop :: FilePath -> [FilePath] -> TVar Pool -> Maybe TargetId -> IO (JobId, WorkerId, HandleSet)
assignLoop ghcPath dbPaths ref mid = untilJustM $ do
  eassigned <- atomically $ assignJob ref mid
  case eassigned of
    Left n -> do
      putStrLn $ "currently " ++ show n ++ " jobs are running. I am spawning a worker."
      _ <- spawnWorker ghcPath dbPaths ref
      pure Nothing
    Right (j, i, hset) -> do
      putStrLn $ "Job assigned with ID: " ++ show j
      pure (Just (j, i, hset))

serve :: FilePath -> [FilePath] -> (TVar Pool, TVar JobStatus) -> Socket -> IO ()
serve ghcPath dbPaths (poolRef, jobStatusRef) s = do
  !msg <- recvMsg s
  let req :: Request = unwrapMsg msg
      mid = requestWorkerTargetId req
  (j, i, hset) <- assignLoop ghcPath dbPaths poolRef mid
  
  res <- runReaderT (work req) (j, i, hset, poolRef, jobStatusRef)
  sendMsg s (wrapMsg res)
  when (requestWorkerClose req) $
    case mid of
      Nothing -> pure ()
      Just id' -> removeWorker poolRef id'
  dumpStatus poolRef
  serve ghcPath dbPaths (poolRef, jobStatusRef) s
