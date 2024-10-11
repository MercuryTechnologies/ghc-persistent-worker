{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Concurrent (forkFinally, forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import qualified Control.Exception as E
import Control.Monad (forever, replicateM_, void, when)
import Control.Monad.Extra (untilJustM)
import qualified Data.IntMap as IM
import Message (Id, Request (..), Response (..), recvMsg, sendMsg, unwrapMsg, wrapMsg)
import Network.Socket
import Options.Applicative (Parser, (<**>))
import qualified Options.Applicative as OA
import Pool (HandleSet (..), Pool (..), assignJob, dumpStatus, finishJob, removeWorker)
import System.IO (Handle, hFlush, hGetLine, hPutStrLn)
import System.Process (CreateProcess (std_in, std_out), StdStream (CreatePipe), createProcess, proc)

runServer :: FilePath -> (Socket -> IO a) -> IO a
runServer fp server = do
  putStrLn "Start serving"
  withSocketsDo $ E.bracket (open fp) close loop
  where
    open f = E.bracketOnError (socket AF_UNIX Stream 0) close $ \sock -> do
        bind sock (SockAddrUnix f)
        listen sock 1024
        return sock
    loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $
            forkFinally (server conn) (const $ gracefulClose conn 5000)

initWorker :: FilePath -> [FilePath] -> Int -> IO HandleSet
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
  (Just hstdin, Just hstdout, _, _) <- createProcess proc_setup
  let hset = HandleSet
        { handleArgIn = hstdin,
          handleMsgOut = hstdout
        }
  pure hset

spawnWorker :: FilePath -> [FilePath] -> TVar Pool -> IO (Int, HandleSet)
spawnWorker ghcPath dbPaths ref = do
  i <- atomically $ do
    pool <- readTVar ref
    let i = poolNext pool
    writeTVar ref (pool {poolNext = i + 1})
    pure i
  hset <- initWorker ghcPath dbPaths i
  atomically $ do
    pool <- readTVar ref
    let s = poolStatus pool
        s' = IM.insert i (False, Nothing) s
        ihsets = poolHandles pool
        ihsets' = (i, hset) : ihsets
    writeTVar ref (pool {poolStatus = s', poolHandles = ihsets'})
  pure (i, hset)

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

work :: (Int, HandleSet) -> Request -> IO Response
work (i, hset) req = do
  let env = requestEnv req
      args = requestArgs req
  -- putStrLn $ "id' = " ++ show id'
  -- putStrLn $ "worker = " ++ show i ++ " will handle this req."
  let hi = handleArgIn hset
      ho = handleMsgOut hset
  hPutStrLn hi (show (env, args))
  hFlush hi
  var <- newEmptyMVar
  _ <- forkIO $ do
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
  pure res

assignLoop :: FilePath -> [FilePath] -> TVar Pool -> Maybe Id -> IO (Int, HandleSet)
assignLoop ghcPath dbPaths ref mid = untilJustM $ do
  eassigned <- atomically $ assignJob ref mid
  case eassigned of
    Left n -> do
      putStrLn $ "currently " ++ show n ++ " jobs are running. I am spawning a worker."
      _ <- spawnWorker ghcPath dbPaths ref
      pure Nothing
      -- atomically $ assignJob ref mid
    Right (i, hset) -> pure (Just (i, hset))


serve :: FilePath -> [FilePath] -> TVar Pool -> Socket -> IO ()
serve ghcPath dbPaths ref s = do
  !msg <- recvMsg s
  let req :: Request = unwrapMsg msg
      mid = requestWorkerId req
  (i, hset) <- assignLoop ghcPath dbPaths ref mid
  res <- work (i, hset) req
  atomically $ finishJob ref i
  sendMsg s (wrapMsg res)
  when (requestWorkerClose req) $
    case mid of
      Nothing -> pure ()
      Just id' -> atomically $ removeWorker ref id'
  dumpStatus ref
  serve ghcPath dbPaths ref s

-- cli args

data Option = Option
  { optionNumWorkers :: Int,
    optionGHC :: FilePath,
    optionSocket :: FilePath,
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
    <*> OA.strOption
        ( OA.long "socket-file"
            <> OA.help "UNIX socket file accepting compilation requests"
        )
    <*> OA.many (OA.strOption (OA.long "package-db" <> OA.help "Package DB Path"))

main :: IO ()
main = do
  opts <- OA.execParser (OA.info (p_option <**> OA.helper) OA.fullDesc)
  let n = optionNumWorkers opts
      ghcPath = optionGHC opts
      socketPath = optionSocket opts
      dbPaths = optionPkgDbs opts
  let thePool = Pool
        { poolLimit = n,
          poolNext = 1,
          poolStatus = IM.empty,
          poolHandles = []
        }

  ref <- newTVarIO thePool
  replicateM_ n $ spawnWorker ghcPath dbPaths ref
  runServer socketPath (serve ghcPath dbPaths ref)
