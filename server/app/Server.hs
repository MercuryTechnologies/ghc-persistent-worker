{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Concurrent (forkFinally, forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, newTVarIO)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import qualified Data.IntMap as IM
import Message (Request (..), Response (..), recvMsg, sendMsg, unwrapMsg, wrapMsg)
import Network.Socket
import Options.Applicative (Parser, (<**>))
import qualified Options.Applicative as OA
import Pool (HandleSet (..), Pool (..), assignJob, dumpStatus, finishJob)
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
  let req :: Request = unwrapMsg msg
      mid = requestWorkerId req
      env = requestEnv req
      args = requestArgs req
  (i, hset) <- atomically $ assignJob ref mid
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
  dumpStatus ref
  --
  atomically $ finishJob ref i
  sendMsg s (wrapMsg res)
  serve ref s

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
      workers = [1..n]
  handles <- traverse (\i -> (i,) <$> initWorker ghcPath dbPaths i) workers
  let thePool = Pool
        { poolStatus = IM.fromList $ map (,(False, Nothing)) workers,
          poolHandles = handles
        }

  ref <- newTVarIO thePool
  runServer socketPath (serve ref)
