{-# OPTIONS_GHC -w #-}
module Worker
( Mailbox (..),
  mailboxForWorker,
  work,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
  ( STM,
    TChan,
    TVar,
    atomically,
    modifyTVar,
    newTChan,
    readTChan,
    writeTChan,
    readTVar,
    retry,
    writeTVar,
  )
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import qualified Data.IntMap as IM
import qualified Data.List as L
import Pool (HandleSet (..), JobId (..), Mailbox (..), Pool (..), WorkerId, finishJob)
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

mailboxForWorker :: TVar Pool -> WorkerId -> Handle -> IO ()
mailboxForWorker poolRef wid hout = void (forkIO $ forever go)
  where
    blockUntilActive = do
      pool <- readTVar poolRef
      case fst <$> IM.lookup wid (poolStatus pool) of
        Just n | n > 0 -> pure ()
        _ -> retry
    sendResult jobid res = do
      pool <- readTVar poolRef
      let hsets = poolHandles pool
      case L.lookup wid hsets of
        Nothing -> retry
        Just hset -> do
          let Mailbox lst = handleMailbox hset
          case L.lookup (JobId jobid) lst of
            Nothing -> retry
            Just chan -> writeTChan chan res
    go = do
      atomically blockUntilActive
      jobid <- read . unlines <$> fetchUntil "*J*O*B*I*D*" hout
      -- get stdout until delimiter
      console_stdout <- fetchUntil "*S*T*D*O*U*T*" hout
      -- get result metatdata until delimiter
      results <- fetchUntil "*R*E*S*U*L*T*" hout
      -- get stderr until delimiter
      console_stderr <- fetchUntil "*D*E*L*I*M*I*T*E*D*" hout
      let res = Response results console_stdout console_stderr
      atomically $ sendResult jobid res

addJobChan :: TVar Pool -> WorkerId -> JobId -> STM (TChan Response, HandleSet)
addJobChan poolRef wid jid = do
  pool <- readTVar poolRef
  let hsets = poolHandles pool
  case L.lookup wid hsets of
    Nothing -> retry
    Just hset -> do
      chan <- newTChan
      let Mailbox lst = handleMailbox hset
          hset' = hset {handleMailbox = Mailbox ((jid, chan) : lst)}
          hsets' = (wid, hset') : (filter ((/= wid) . fst) hsets)
      writeTVar poolRef pool {poolHandles = hsets'}
      pure (chan, hset')

removeJobChan :: TVar Pool -> WorkerId -> JobId -> STM ()
removeJobChan poolRef wid jid =
  modifyTVar poolRef $ \pool ->
    let hsets = poolHandles pool
     in case L.lookup wid hsets of
       Nothing -> pool
       Just hset ->
         let Mailbox lst = handleMailbox hset
             lst' = filter ((jid /=) . fst) lst
             hset' = hset {handleMailbox = Mailbox lst'}
             hsets' = (wid, hset') : (filter ((/= wid) . fst) hsets)
          in pool {poolHandles = hsets'}

sendRequest :: Request -> JobM ()
sendRequest req = do
  (JobId jid, _, hset, _) <- ask
  let env = requestEnv req
      args = requestArgs req
  let hi = handleArgIn hset
  liftIO $ do
    hPutStrLn hi (show (env, show jid : args))
    hFlush hi

waitResponse :: TChan Response -> JobM Response
waitResponse chan = do
  (jid, wid, _, poolRef) <- ask
  liftIO $ atomically $ do
    r <- readTChan chan
    finishJob poolRef wid
    removeJobChan poolRef wid jid
    pure r

work :: Request -> JobM Response
work req = do
  (jid, wid, hset, poolRef) <- ask
  (chan, _) <- liftIO $ atomically $ addJobChan poolRef wid jid

  sendRequest req
  res@(Response results _ _) <- waitResponse chan

  liftIO $ putStrLn $ "worker " ++ show wid ++ " returns: " ++ show results
  pure res
