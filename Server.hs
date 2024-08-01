{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Concurrent (forkFinally, threadDelay)
import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVar, retry, writeTVar)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void, when)
import Data.Binary (decode)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Foldable (traverse_)
import Data.Int (Int32)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.List as List
import Message (Msg (..), recvMsg, sendMsg, unwrapMsg, wrapMsg)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath ((</>), (<.>))
import System.IO (Handle, IOMode (ReadMode, WriteMode), hFlush, hGetLine, hPutStrLn, openFile)
import System.Posix.Files (createNamedPipe, fileAccess, ownerModes)
import System.Posix.IO
  ( OpenFileFlags (nonBlock),
    OpenMode (ReadOnly, WriteOnly),
    defaultFileFlags,
    fdToHandle,
    openFd,
  )
import System.Process (CreateProcess (std_in, std_out), StdStream (CreatePipe), createProcess, proc)
import Util (openFileAfterCheck, openPipeRead, openPipeWrite, whenM)

data Pool = Pool
  { poolStatus :: IntMap Bool,
    poolHandles :: [(Int, (Handle, Handle))]
  }

runServer :: FilePath -> (Socket -> IO a) -> IO a
runServer fp server = do
  putStrLn "Start serving"
  withSocketsDo $ E.bracket (open fp) close loop
  where
    open fp = E.bracketOnError (socket AF_UNIX Stream 0) close $ \sock -> do
        bind sock (SockAddrUnix fp)
        listen sock 5
        return sock
    loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $
            forkFinally (server conn) (const $ gracefulClose conn 5000)

assignJob :: TVar Pool -> STM (Int, (Handle, Handle))
assignJob ref = do
  pool <- readTVar ref
  let workers = poolStatus pool
  let m = List.find ((== False) . snd) (IM.toAscList workers)
  case m of
    Nothing -> retry
    Just (i, _) -> do
      let !workers' = IM.update (\_ -> Just True) i workers
          Just (hi, ho) = List.lookup i (poolHandles pool)
      writeTVar ref (pool {poolStatus = workers'})
      pure (i, (hi, ho))

finishJob :: TVar Pool -> Int -> STM ()
finishJob ref i = do
  pool <- readTVar ref
  let workers = poolStatus pool
      !workers' = IM.update (\_ -> Just False) i workers
  writeTVar ref (pool {poolStatus = workers'})

initWorker :: Int -> IO (Handle, Handle)
initWorker i = do
  putStrLn $ "worker " ++ show i ++ " is initialized"
  cwd <- getCurrentDirectory
  let exec_path = "ghc" -- cwd </> "Worker"
      infile = cwd </> "in" ++ show i <.> "fifo"
      outfile = cwd </> "out" ++ show i <.> "fifo"
      ghc_options =
        [ "-package-db",
          "myplugin/dist-newstyle/packagedb/ghc-9.6.5",
          "--frontend",
          "MyPlugin",
          "-ffrontend-opt",
          show i,
          "-ffrontend-opt",
          infile,
          "-ffrontend-opt",
          outfile
        ]
      proc_setup = (proc exec_path ghc_options)
  whenM (not <$> doesFileExist infile) (createNamedPipe infile ownerModes)
  whenM (not <$> doesFileExist outfile) (createNamedPipe outfile ownerModes)
  ho <- openFileAfterCheck outfile (True, False) openPipeRead
  _ <- createProcess proc_setup
  hi <- openFileAfterCheck infile (False, True) openPipeWrite
  pure (hi, ho)

serve :: TVar Pool -> Socket -> IO ()
serve ref s = do
  msg <- recvMsg s
  (i, (hi, ho)) <- atomically $ assignJob ref
  let xs :: [String] = unwrapMsg msg
  putStrLn $ "worker = " ++ show i ++ ": " ++ show xs
  let n' = length xs
      msg' = wrapMsg n'
  -- simulate a worker
  hPutStrLn hi (show xs)
  hFlush hi
  results <- hGetLine ho
  putStrLn $ "worker " ++ show i ++ " returns: " ++ results
  --
  atomically $ finishJob ref i
  sendMsg s msg'
  serve ref s

main :: IO ()
main = do
  let workers = [1, 2, 3, 4]
  handles <- traverse (\i -> (i,) <$> initWorker i) workers
  let thePool = Pool
        { poolStatus = IM.fromList $ map (,False) workers,
          poolHandles = handles
        }

  ref <- newTVarIO thePool
  runServer "/tmp/mytest.ipc" (serve ref)
