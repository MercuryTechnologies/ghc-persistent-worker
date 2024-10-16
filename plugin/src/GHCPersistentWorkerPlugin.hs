{-# LANGUAGE NumericUnderscores #-}
module GHCPersistentWorkerPlugin (frontendPlugin) where

import Control.Concurrent (forkOS)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forever, replicateM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import Data.Foldable (for_)
import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime)
import qualified GHC
import GHC.Driver.Env (HscEnv (hsc_NC, hsc_interp, hsc_logger, hsc_unit_env))
import GHC.Driver.Monad (Ghc, Session, withSession, withTempSession, modifySession, reflectGhc, reifyGhc)
import GHC.Driver.Plugins (FrontendPlugin (..), defaultFrontendPlugin)
import GHC.Main
  ( PreStartupMode (..),
    main',
    parseModeFlags,
    showSupportedExtensions,
    showVersion,
    showOptions,
  )
import GHC.Settings.Config (cProjectVersion)
import GHC.Utils.Logger (pushLogHook)
import Logger (logHook)
import System.Directory (getTemporaryDirectory, removeFile, setCurrentDirectory)
import System.Environment (setEnv)
import System.FilePath ((</>))
import System.IO
  ( BufferMode (..),
    Handle,
    IOMode (..),
    hFlush,
    hGetLine,
    hPutStrLn,
    hSetBuffering,
    stdin,
    stderr,
    stdout,
    withFile,
  )
import Util (openFileAfterCheck, openPipeRead, openPipeWrite)


frontendPlugin :: FrontendPlugin
frontendPlugin = defaultFrontendPlugin
  {
    frontend = \flags _args -> workerMain flags
  }

logMessage :: String -> IO ()
logMessage = hPutStrLn stderr

prompt :: Int -> String
prompt wid = "[Worker:" ++ show wid ++ "]"

recvRequestFromServer :: Handle -> Int -> IO (Int, [(String, String)], [String])
recvRequestFromServer hin wid = do
  s <- hGetLine hin
  let (env, args0) :: ([(String, String)], [String]) = read s
  let jobid_str : args = args0
      jobid :: Int
      jobid = read jobid_str
  logMessage (prompt wid ++ " job id = " ++ show jobid)
  logMessage (prompt wid ++ " Got args: " ++ intercalate " " args)
  pure (jobid, env, args)

setEnvForJob :: [(String, String)] -> IO ()
setEnvForJob env = do
  for_ (lookup "PWD" env) setCurrentDirectory
  for_ env $ \(var, val) -> setEnv var val

sendJobIdToServer :: Handle -> Int -> IO ()
sendJobIdToServer hout jobid = do
  hPutStrLn hout (show jobid)
  hPutStrLn hout "*J*O*B*I*D*"

sendResultToServer :: Handle -> String -> B.ByteString -> IO ()
sendResultToServer hout result bs = do
  B.hPut hout bs
  hPutStrLn hout "" -- important to delimit with a new line.
  hPutStrLn hout "*S*T*D*O*U*T*"
  hFlush hout
  hPutStrLn hout result
  hPutStrLn hout "*R*E*S*U*L*T*"
  hFlush hout
  hPutStrLn hout "*D*E*L*I*M*I*T*E*D*"
  hFlush hout

bannerJobStart :: Int -> IO ()
bannerJobStart wid = do
  mapM_ (\_ -> hPutStrLn stderr "=================================") [1..5]
  time <- getCurrentTime
  hPutStrLn stderr $ "worker: " ++ (show wid)
  hPutStrLn stderr (show time)
  mapM_ (\_ -> hPutStrLn stderr "=================================") [1..5]

bannerJobEnd :: Int -> IO ()
bannerJobEnd wid = do
  mapM_ (\_ -> hPutStrLn stderr "|||||||||||||||||||||||||||||||||") [1..5]
  time <- getCurrentTime
  hPutStrLn stderr $ "worker: " ++ (show wid)
  hPutStrLn stderr (show time)
  mapM_ (\_ -> hPutStrLn stderr "|||||||||||||||||||||||||||||||||") [1..5]

withTempLogger :: Session -> Int -> Int -> (Ghc a) -> IO (B.ByteString, a)
withTempLogger session wid jobid action = do
  tmpdir <- getTemporaryDirectory
  let file_stdout = tmpdir </> "ghc-worker-tmp-logger-" ++ show wid ++ "-" ++ show jobid ++ "-stdout.log"
      file_stderr = tmpdir </> "ghc-worker-tmp-logger-" ++ show wid ++ "-" ++ show jobid ++ "-stderr.log"
  r <-
    withFile file_stdout WriteMode $ \nstdout ->
      withFile file_stderr WriteMode $ \nstderr -> do
        r <-
          flip reflectGhc session $
            withTempSession
              (\env ->
                 env {
                   hsc_logger = pushLogHook (logHook (nstdout, nstderr)) (hsc_logger env)
                 }
              )
              action
        hFlush nstdout
        hFlush nstderr
        pure r
  bs <-
    withFile file_stdout ReadMode $ \nstdout -> do
      -- TODO: stderr as well
      bs <- B.hGetContents nstdout
      replicateM_ 5 (hPutStrLn stderr "****************")
      B.hPut stderr bs
      replicateM_ 5 (hPutStrLn stderr "****************")
      pure bs
  removeFile file_stdout
  removeFile file_stderr
  pure (bs, r)


workerMain :: [String] -> Ghc ()
workerMain flags = do
  liftIO $ do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

  let wid :: Int = read (flags !! 0)
      (hin, hout) = (stdin, stdout)
  liftIO $ logMessage (prompt wid ++ " Started")
  GHC.initGhcMonad Nothing
  forever $ do
    lock <- liftIO newEmptyMVar
    reifyGhc $ \session -> forkOS $ do
      (jobid, env, args) <- recvRequestFromServer hin wid
      setEnvForJob env
      sendJobIdToServer hout jobid
      --
      bannerJobStart wid
      (bs, _) <- withTempLogger session wid jobid (compileMain args)
      bannerJobEnd wid
      --
      -- TODO: will have more useful info
      let result = "DUMMY RESULT"
      sendResultToServer hout result bs
      putMVar lock ()
      --
    () <- liftIO $ takeMVar lock
    (minterp, unit_env, nc) <-
      withSession $ \env ->
        pure $ (hsc_interp env, hsc_unit_env env, hsc_NC env)
    GHC.initGhcMonad Nothing
    modifySession $ \env ->
      env
        { hsc_interp = minterp,
          -- hsc_unit_env = unit_env,
          hsc_NC = nc
        }
    liftIO $ putMVar lock ()


compileMain :: [String] -> Ghc ()
compileMain args = do
  let argv2 = map (GHC.mkGeneralLocated "on the commandline") args
  (mode, units, argv3, flagWarnings) <- liftIO $ parseModeFlags argv2

  dflags0 <- GHC.getSessionDynFlags
  case mode of
    Left ShowSupportedExtensions     ->
      liftIO $ showSupportedExtensions Nothing
    Left ShowVersion                 ->
      liftIO $ showVersion
    Left ShowNumVersion              ->
      liftIO $ putStrLn cProjectVersion
    Left (ShowOptions isInteractive) ->
      liftIO $ showOptions isInteractive
    Right (Right postLoadMode) ->
      main' postLoadMode units dflags0 argv3 flagWarnings
    _ -> pure ()
