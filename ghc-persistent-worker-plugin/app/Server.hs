{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Concurrent (forkFinally, forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
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
import Message (ConsoleOutput (..), Msg (..), recvMsg, sendMsg, unwrapMsg, wrapMsg)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>), (<.>))
import System.IO (Handle, IOMode (ReadMode, WriteMode), hFlush, hGetContents, hGetLine, hPutStrLn, openFile)
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

data HandleSet = HandleSet
  { handleArgIn :: Handle,
    handleMsgOut :: Handle,
    handleStdOut :: Handle
  }

data Pool = Pool
  { poolStatus :: IntMap Bool,
    poolHandles :: [(Int, HandleSet)]
  }

runServer :: FilePath -> (Socket -> IO a) -> IO a
runServer fp server = do
  putStrLn "Start serving"
  withSocketsDo $ E.bracket (open fp) close loop
  where
    open fp = E.bracketOnError (socket AF_UNIX Stream 0) close $ \sock -> do
        bind sock (SockAddrUnix fp)
        listen sock 1024
        return sock
    loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $
            forkFinally (server conn) (const $ gracefulClose conn 5000)

assignJob :: TVar Pool -> STM (Int, HandleSet)
assignJob ref = do
  pool <- readTVar ref
  let workers = poolStatus pool
  let m = List.find ((== False) . snd) (IM.toAscList workers)
  case m of
    Nothing -> retry
    Just (i, _) -> do
      let !workers' = IM.update (\_ -> Just True) i workers
          Just hset = List.lookup i (poolHandles pool)
      writeTVar ref (pool {poolStatus = workers'})
      pure (i, hset)

finishJob :: TVar Pool -> Int -> STM ()
finishJob ref i = do
  pool <- readTVar ref
  let workers = poolStatus pool
      !workers' = IM.update (\_ -> Just False) i workers
  writeTVar ref (pool {poolStatus = workers'})

initWorker :: Int -> IO HandleSet
initWorker i = do
  putStrLn $ "worker " ++ show i ++ " is initialized"
  cwd <- getCurrentDirectory
  home <- getHomeDirectory
  let exec_path = "ghc"
      infile = cwd </> "in" ++ show i <.> "fifo"
      outfile = cwd </> "out" ++ show i <.> "fifo"
      ghc_options =
        [ "-package-db",
          (home </> ".local/state/cabal/store/ghc-9.11.20240809/package.db"),
          "-package-db",
          (home </> "repo/mercury/ghc-persistent-worker/dist-newstyle/packagedb/ghc-9.11.20240809"),
          "-plugin-package",
          "ghc-persistent-worker-plugin",
          "--frontend",
          "GHCPersistentWorkerPlugin",
          "-ffrontend-opt",
          show i,
          "-ffrontend-opt",
          infile,
          "-ffrontend-opt",
          outfile
        ]
      proc_setup =
        (proc exec_path ghc_options)
          { std_out = CreatePipe
          }
  whenM (not <$> doesFileExist infile) (createNamedPipe infile ownerModes)
  whenM (not <$> doesFileExist outfile) (createNamedPipe outfile ownerModes)
  ho <- openFileAfterCheck outfile (True, False) openPipeRead
  (_, Just hstdout, _, _) <- createProcess proc_setup
  hi <- openFileAfterCheck infile (False, True) openPipeWrite
  let hset = HandleSet
        { handleArgIn = hi,
          handleMsgOut = ho,
          handleStdOut = hstdout
        }
  pure hset

fetchUntil :: String -> Handle -> IO [String]
fetchUntil delim h = do
    f <- go id
    pure (f [])
  where
    go acc = do
      s <- hGetLine h
      if s == delim
        then pure acc
        else go (acc . (s:))

serve :: TVar Pool -> Socket -> IO ()
serve ref s = do
  !msg <- recvMsg s
  (i, hset) <- atomically $ assignJob ref
  let xs :: [String] = unwrapMsg msg
  putStrLn $ "worker = " ++ show i ++ ": " ++ show xs
  let hi = handleArgIn hset
      ho = handleMsgOut hset
      hstdout = handleStdOut hset
  hPutStrLn hi (show xs)
  hFlush hi
  -- get stdout until delimiter
  var <- newEmptyMVar
  forkIO $ do
    consoleOutput <- fetchUntil "*D*E*L*I*M*I*T*E*D*" hstdout
    putMVar var consoleOutput

  results <- hGetLine ho
  putStrLn $ "worker " ++ show i ++ " returns: " ++ results
  consoleOutput <- takeMVar var
  --
  atomically $ finishJob ref i
  sendMsg s (wrapMsg (ConsoleOutput consoleOutput))
  serve ref s

main :: IO ()
main = do
  args <- getArgs
  let n :: Int = read (args !! 0)
  let workers = [1..n]
  handles <- traverse (\i -> (i,) <$> initWorker i) workers
  let thePool = Pool
        { poolStatus = IM.fromList $ map (,False) workers,
          poolHandles = handles
        }

  ref <- newTVarIO thePool
  runServer "/tmp/mytest.ipc" (serve ref)
