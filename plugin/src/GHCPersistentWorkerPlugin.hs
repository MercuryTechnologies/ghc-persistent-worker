{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
module GHCPersistentWorkerPlugin (frontendPlugin) where

import Control.Concurrent (forkOS)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Monad (forever, replicateM_, void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import Data.Foldable (for_)
import qualified Data.Knob
import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime)
import qualified GHC
import GHC.Driver.Env (HscEnv (hsc_NC, hsc_interp, hsc_logger, hsc_unit_env))
import GHC.Driver.Monad (Ghc, Session, withSession, withTempSession, modifySession, reflectGhc, reifyGhc)
import GHC.Driver.Plugins (FrontendPlugin (..), defaultFrontendPlugin)
import qualified GHC.Linker.Loader as Loader
import GHC.Main
  ( PreStartupMode (..),
    main',
    parseModeFlags,
    showSupportedExtensions,
    showVersion,
    showOptions,
  )
import GHC.Runtime.Interpreter.Types (Interp (..), InterpInstance (..))
import GHC.Settings.Config (cProjectVersion)
import GHC.Types.Name.Cache (NameCache)
import GHC.Types.Unique.FM (emptyUFM)
import GHC.Utils.Logger (pushLogHook)
import Logger (logHook)
import System.Directory (getTemporaryDirectory, removeFile, setCurrentDirectory)
import System.Environment (setEnv)
import System.FilePath ((</>))
import System.IO
  ( BufferMode (..),
    Handle,
    IOMode (..),
    hClose,
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
  -- logMessage (prompt wid ++ " job id = " ++ show jobid)
  -- logMessage (prompt wid ++ " Got args: " ++ intercalate " " args)
  pure (jobid, env, args)

setEnvForJob :: [(String, String)] -> IO ()
setEnvForJob env = do
  for_ (lookup "PWD" env) setCurrentDirectory
  for_ env $ \(var, val) -> setEnv var val

sendResultToServer :: MVar Handle -> Int -> String -> B.ByteString -> IO ()
sendResultToServer chanOut jobid result bs = do
  hout <- takeMVar chanOut
  hPutStrLn hout "*S*T*A*R*T*"
  hFlush hout
  hPutStrLn hout (show jobid)
  hPutStrLn hout "*J*O*B*I*D*"
  hFlush hout
  B.hPut hout bs
  hPutStrLn hout "" -- important to delimit with a new line.
  hPutStrLn hout "*S*T*D*O*U*T*"
  hFlush hout
  hPutStrLn hout result
  hPutStrLn hout "*R*E*S*U*L*T*"
  hFlush hout
  hPutStrLn hout "*D*E*L*I*M*I*T*E*D*"
  hFlush hout
  putMVar chanOut hout

bannerJobStart :: Int -> IO ()
bannerJobStart wid = do
  replicateM_ 5 (hPutStrLn stderr "=================================")
  time <- getCurrentTime
  hPutStrLn stderr $ "worker: " ++ (show wid)
  hPutStrLn stderr (show time)
  replicateM_ 5 (hPutStrLn stderr "=================================")

bannerJobEnd :: Int -> IO ()
bannerJobEnd wid = do
  replicateM_ 5 (hPutStrLn stderr "|||||||||||||||||||||||||||||||||")
  time <- getCurrentTime
  hPutStrLn stderr $ "worker: " ++ (show wid)
  hPutStrLn stderr (show time)
  replicateM_ 5 (hPutStrLn stderr "|||||||||||||||||||||||||||||||||")

withTempLogger :: Session -> Int -> Int -> (Ghc a) -> IO B.ByteString
withTempLogger session wid jobid action = do
  let file_stdout = "ghc-worker-tmp-logger-" ++ show wid ++ "-" ++ show jobid ++ "-stdout.log"
      file_stderr = "ghc-worker-tmp-logger-" ++ show wid ++ "-" ++ show jobid ++ "-stderr.log"
  knob_out <- Data.Knob.newKnob B.empty
  knob_err <- Data.Knob.newKnob B.empty
  nstdout <- Data.Knob.newFileHandle knob_out file_stdout WriteMode
  nstderr <- Data.Knob.newFileHandle knob_err file_stderr WriteMode
  -- let nstderr = stderr
  -- for now
  -- withFile "/dev/null" AppendMode $ \nstderr -> do
  flip reflectGhc session $
    withTempSession
      (\env ->
         env {
           hsc_logger = pushLogHook (logHook (nstdout, nstderr)) (hsc_logger env)
         }
      )
      action
  -- hFlush nstdout
  hClose nstdout
  hClose nstderr
  -- hFlush nstderr
  bs <- Data.Knob.getContents knob_out
  bs2 <- Data.Knob.getContents knob_err
  pure (bs <> "\n" <> bs2)

loopShot :: Interp -> NameCache -> Handle -> MVar Handle -> Int -> Ghc ()
loopShot interp nc hin chanOut wid = do
  (jobid, env, args) <- liftIO $ recvRequestFromServer hin wid
  liftIO $ hPutStrLn stderr ("In loopShot: " ++ show (jobid, args))
  let isShowIfaceAbiHash = any (== "--show-iface-abi-hash") args

  modifySession $ \env ->
    env
      { hsc_interp = Just interp,
        hsc_NC = nc
      }
  lock <- liftIO $ newEmptyMVar
  reifyGhc $ \session -> void $ forkOS $ do
    setEnvForJob env
    --
    -- bannerJobStart wid
    bs <- withTempLogger session wid jobid (compileMain args)
    -- bannerJobEnd wid
    --
    -- TODO: will have more useful info
    let result = "DUMMY RESULT"
    sendResultToServer chanOut jobid result bs
    reflectGhc (GHC.initGhcMonad Nothing) session
    putMVar lock ()
  -- AD HOC TREATMENT
  when isShowIfaceAbiHash $ liftIO $ takeMVar lock

workerMain :: [String] -> Ghc ()
workerMain flags = do
  liftIO $ do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

  let wid :: Int = read (flags !! 0)
      (hin, hout) = (stdin, stdout)
  -- liftIO $ logMessage (prompt wid ++ " Started")
  GHC.initGhcMonad Nothing
  -- explicitly initialize loader.
  lookup_cache <- liftIO $ newMVar emptyUFM
  loader <- liftIO Loader.uninitializedLoader
  let interp = Interp InternalInterp loader lookup_cache
  nc <- hsc_NC <$> GHC.getSession
  -- exclusive message channel
  chanOut <- liftIO $ newMVar hout
  forever $ loopShot interp nc hin chanOut wid

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
