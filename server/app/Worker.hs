{-# OPTIONS_GHC -w #-}
module Worker
( Mailbox (..),
  mailboxForWorker,
  work,
) where

import Control.Concurrent (forkIO)
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
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import qualified Data.List as L
import Pool (HandleSet (..), JobId, Mailbox (..), Pool (..), WorkerId, finishJob)
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

mailboxForWorker :: TVar Pool -> IO ()
mailboxForWorker _poolRef = pure ()

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

work :: Request -> JobM Response
work req = do
  (jid, wid, hset, poolRef) <- ask
  let env = requestEnv req
      args = requestArgs req
  let hi = handleArgIn hset
      ho = handleMsgOut hset
  liftIO $ print jid
  liftIO $ do
    hPutStrLn hi (show (env, args))
    hFlush hi
  (chan, _) <- liftIO $ atomically $ addJobChan poolRef wid jid
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
    finishJob poolRef wid
    -- modifyTVar mailboxRef $ \(Mailbox lst) ->
    --   let lst' = filter ((j /=) . fst) lst
    --    in Mailbox lst'
    pure r

  liftIO $ putStrLn $ "worker " ++ show wid ++ " returns: " ++ show results
  pure res
