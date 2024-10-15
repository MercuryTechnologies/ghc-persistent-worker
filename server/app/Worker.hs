module Worker (work) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TVar, atomically, newTChanIO, readTChan, writeTChan)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Pool (HandleSet (..), JobId, Pool, WorkerId, finishJob)
import Message (Request (..), Response (..))
import System.IO (Handle, hFlush, hGetLine, hPutStrLn)

type JobM = ReaderT (JobId, WorkerId, HandleSet, TVar Pool) IO

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

work :: Request -> JobM Response
work req = do
  (j, i, hset, ref) <- ask
  let env = requestEnv req
      args = requestArgs req
  let hi = handleArgIn hset
      ho = handleMsgOut hset
  liftIO $ print j
  liftIO $ do
    hPutStrLn hi (show (env, args))
    hFlush hi
  chan <- liftIO newTChanIO
  _ <- liftIO $ forkIO $ do
    -- get stdout until delimiter
    console_stdout <- fetchUntil "*S*T*D*O*U*T*" ho
    -- get result metatdata until delimiter
    results <- fetchUntil "*R*E*S*U*L*T*" ho
    -- get stderr until delimiter
    console_stderr <- fetchUntil "*D*E*L*I*M*I*T*E*D*" ho
    let res = Response results console_stdout console_stderr
    -- putMVar var res
    atomically $ writeTChan chan res

  res@(Response results _ _) <- liftIO $ atomically $ do
    r <- readTChan chan
    finishJob ref i
    pure r

  liftIO $ putStrLn $ "worker " ++ show i ++ " returns: " ++ show results
  pure res
