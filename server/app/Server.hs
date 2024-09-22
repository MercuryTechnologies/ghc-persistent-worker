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
import Message (Msg (..), Request (..), Response (..), recvMsg, sendMsg, unwrapMsg, wrapMsg)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Options.Applicative (Parser, (<**>))
import qualified Options.Applicative as OA
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
    handleMsgOut :: Handle
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

initWorker :: FilePath -> [FilePath] -> Int -> IO HandleSet
initWorker ghcPath dbPaths i = do
  putStrLn $ "worker " ++ show i ++ " is initialized"
  cwd <- getCurrentDirectory
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
  (Just hstdin, Just hstdout, _, _) <- createProcess proc_setup
  let hset = HandleSet
        { handleArgIn = hstdin,
          handleMsgOut = hstdout
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
  let req :: Request = unwrapMsg msg
      env = requestEnv req
      args = requestArgs req
  putStrLn $ "worker = " ++ show i ++ " will handle this req."
  let hi = handleArgIn hset
      ho = handleMsgOut hset
  hPutStrLn hi (show (env, args))
  hFlush hi
  var <- newEmptyMVar
  forkIO $ do
    -- get stdout until delimiter
    console_stdout <- fetchUntil "*S*T*D*O*U*T*" ho
    -- get result metatdata until delimiter
    results <- fetchUntil "*R*E*S*U*L*T*" ho
    -- get stderr until delimiter
    console_stderr <- fetchUntil "*D*E*L*I*M*I*T*E*D*" ho
    let res = Response results console_stdout console_stderr
    putMVar var res

  res@(Response results _ _) <- takeMVar var

  putStrLn $ "worker " ++ show i ++ " returns: " ++ show results
  --
  atomically $ finishJob ref i
  sendMsg s (wrapMsg res)
  serve ref s

-- cli args

data Option = Option
  { optionNumWorkers :: Int,
    optionGHC :: FilePath,
    optionPkgDbs :: [FilePath]
  }

p_option :: Parser Option
p_option =
  Option
    <$> OA.option OA.auto
        ( OA.long "num"
            <> OA.short 'n'
            <> OA.help "number of workers"
        )
    <*> OA.strOption
        ( OA.long "ghc"
            <> OA.help "GHC path"
        )
    <*> OA.many (OA.strOption (OA.long "package-db" <> OA.help "Package DB Path"))

main :: IO ()
main = do
  opts <- OA.execParser (OA.info (p_option <**> OA.helper) OA.fullDesc)
  let n = optionNumWorkers opts
      ghcPath = optionGHC opts
      dbPaths = optionPkgDbs opts
      workers = [1..n]
  handles <- traverse (\i -> (i,) <$> initWorker ghcPath dbPaths i) workers
  let thePool = Pool
        { poolStatus = IM.fromList $ map (,False) workers,
          poolHandles = handles
        }

  ref <- newTVarIO thePool
  runServer "/tmp/mytest.ipc" (serve ref)
