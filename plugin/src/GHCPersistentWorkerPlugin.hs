{-# LANGUAGE NumericUnderscores, CPP #-}
module GHCPersistentWorkerPlugin (frontendPlugin) where

import Control.Concurrent (MVar, forkOS)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forever, replicateM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import Data.Foldable (for_)
import Data.List (intercalate, uncons)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)
import qualified GHC
import GHC.Driver.Env (HscEnv (hsc_NC, hsc_interp, hsc_unit_env))
import GHC.Driver.Monad (Ghc, modifySession, reflectGhc, reifyGhc, withSession)
import GHC.Driver.Plugins (FrontendPlugin (..), defaultFrontendPlugin)
import qualified GHC.Linker.Loader as Loader
import GHC.Main (PreStartupMode (..), main', parseModeFlags, showOptions, showSupportedExtensions, showVersion)
import GHC.Plugins (GhcException (UsageError), throwGhcException)
import GHC.Runtime.Interpreter.Types (Interp (..), InterpInstance (..))
import GHC.Settings.Config (cProjectVersion)
import Internal.Args (Args (..), emptyArgs)
import Internal.Cache (emptyCache)
import Internal.Compile (compile)
import Internal.Error (handleExceptions)
import Internal.Log (Log, logFlushBytes, logToState, newLog)
import Internal.Session (Env (..), withGhc)
import System.Directory (setCurrentDirectory)
import System.Environment (setEnv)
import System.IO (BufferMode (..), Handle, hFlush, hGetLine, hPutStrLn, hSetBuffering, stderr, stdin, stdout)
import Text.Read (readMaybe)

#if __GLASGOW_HASKELL__ >= 911
import Control.Concurrent.MVar (newMVar)
import GHC.Types.Unique.FM (emptyUFM)
#endif

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
  let (jobid_str, args) = fromMaybe (error "empty args in request") (uncons args0)
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

-- | Abstraction of the two worker variants, purely for convenience of sharing the loop logic.
data WorkerImpl where
  WorkerImpl ::
    -- | Initial resource acquisition, called once and passed to each compile job.
    Ghc a ->
    -- | Use the resource to execute a job specified by the argument list.
    (a -> MVar Log -> [String] -> IO Int) ->
    -- | Update the state after a job has concluded.
    Ghc () ->
    WorkerImpl

-- | The conventional implementation, unchanged by the refactoring, hopefully.
workerImplDefault :: WorkerImpl
workerImplDefault =
  WorkerImpl acquire use finalizeJob
  where
    acquire = do
      GHC.initGhcMonad Nothing
      -- explicitly initialize loader.
      loader <- liftIO Loader.uninitializedLoader
#if __GLASGOW_HASKELL__ >= 911
      lookup_cache <- liftIO $ newMVar emptyUFM
      let interp = Interp InternalInterp loader lookup_cache
#else
      let interp = Interp InternalInterp loader
#endif
      modifySession $ \env -> env {hsc_interp = Just interp}
      reifyGhc pure

    use session logVar args = do
      flip reflectGhc session $ do
        GHC.pushLogHookM (const (logToState logVar))
        handleExceptions logVar 1 (0 <$ compileMain args)

    finalizeJob = do
      (minterp, _unit_env, nc) <-
        withSession $ \env ->
          pure $ (hsc_interp env, hsc_unit_env env, hsc_NC env)
      GHC.initGhcMonad Nothing
      modifySession $ \env ->
        env
          { hsc_interp = minterp,
            -- hsc_unit_env = unit_env,
            hsc_NC = nc
          }

-- | Worker implementation that uses the custom session management and compilation pipeline developed in the other
-- worker variant.
-- In contrast to the default implementation, state management is handled entirely within the @Internal@ modules.
--
-- From the Buck Haskell rules, this can be selected by adding @--worker-multiplexer-custom@ to the command args when
-- using the worker.
--
-- From the Buck CLI, this can be selected with @--config ghc-worker.multiplexer_custom=true@, or setting
-- @multiplexer_custom = true@ in a config file under @[ghc-worker]@.
workerImplCustom :: WorkerImpl
workerImplCustom =
  WorkerImpl acquire use finalizeJob
  where
    acquire = liftIO $ emptyCache True

    use cache logVar args = do
      let env' = Env {log = logVar, cache, args = (emptyArgs mempty) {ghcOptions = sanitize args}}
      fmap (fromMaybe 2) $ withGhc env' \ target ->
        compile target >>= \case
          Just _ -> pure (Just 0)
          Nothing -> pure (Just 1)

    finalizeJob = pure ()

    sanitize = \case
      "-c" : rest -> rest
      a -> a

-- | Main loop abstracting over the session/pipeline implementation.
--
-- Repeatedly run the compilation pipeline for each job received from the worker server through the handle @hin@,
-- sending the result back through @hout@.
--
-- Implementations can have a resource acquisition action and an action to be run after each job, for updating the
-- state.
-- This is simply a crude refactoring of the two separate original implementations.
workerLoop ::
  WorkerImpl ->
  -- | Message input handle.
  Handle ->
  -- | Message output handle.
  Handle ->
  -- | Worker ID sent back to the server to match results to clients.
  Int ->
  Ghc ()
workerLoop (WorkerImpl acquire use finalizeJob) hin hout wid = do
  resource <- acquire
  forever $ do
    lock <- liftIO newEmptyMVar
    _ <- liftIO $ forkOS $ do
      (jobid, env, args) <- recvRequestFromServer hin wid
      setEnvForJob env
      sendJobIdToServer hout jobid
      logVar <- newLog False
      --
      bannerJobStart wid
      result <- use resource logVar args
      bannerJobEnd wid
      --
      output <- logFlushBytes logVar
      sendResultToServer hout (show result) output
      putMVar lock ()
      --
    () <- liftIO $ takeMVar lock
    finalizeJob
    liftIO $ putMVar lock ()

data ImplConf = WorkerDefault | WorkerCustom

-- | Parse flags passed to the plugin on invocation:
--
-- - The first argument is a mandatory integer.
--   It denotes the worker ID, corresponding to a Buck target or GHC unit, used to map compilation jobs to clients.
--
-- - The second argument is an optional string that selects the implementation of the compilation pipeline invocation,
--   which can be 'default' to just call GHC's main function or 'custom' to use the local experimental variant.
parseFlags :: [String] -> Either String (Int, ImplConf)
parseFlags = \case
  [] -> Left "Need at least one argument specifying the integer worker ID"
  [widSpec] -> do
    wid <- parseId widSpec
    Right (wid, WorkerDefault)
  [widSpec, implSpec] -> do
    wid <- parseId widSpec
    impl <- parseImpl implSpec
    Right (wid, impl)
  flags -> Left ("Expecting at most two arguments, got: " ++ unwords flags)
  where
    parseId spec =
      case readMaybe spec of
        Just wid -> Right wid
        Nothing -> Left ""

    parseImpl = \case
      "custom" -> Right WorkerCustom
      "default" -> Right WorkerDefault
      wrong -> Left ("Invalid argument for worker impl, should be 'custom' or 'default': " ++ wrong)

-- | Parse the @flags@, which are passed to the plugin from @Server.initWorker@ using @-ffrontend-opt@ when starting a
-- new worker, and start the compilation server loop based on the resulting configuration.
workerMain :: [String] -> Ghc ()
workerMain flags = do
  liftIO $ do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
  case parseFlags flags of
    Left message -> do
      throwGhcException $ UsageError ("Invalid arguments for worker via '-ffrontend-opt': " ++ message)
    Right (wid, impl) -> do
      let handler = \case
            WorkerCustom -> workerImplCustom
            WorkerDefault -> workerImplDefault
      liftIO $ logMessage (prompt wid ++ " Started")
      workerLoop (handler impl) stdin stdout wid

compileMain :: [String] -> Ghc ()
compileMain args = do
  let argv2 = map (GHC.mkGeneralLocated "on the commandline") args
  (impl, units, argv3, flagWarnings) <- liftIO $ parseModeFlags argv2

  dflags0 <- GHC.getSessionDynFlags
  case impl of
    Left ShowSupportedExtensions     ->
      liftIO $ showSupportedExtensions Nothing
    Left ShowVersion                 ->
      liftIO $ showVersion
    Left ShowNumVersion              ->
      liftIO $ putStrLn cProjectVersion
    Left (ShowOptions isInteractive) ->
      liftIO $ showOptions isInteractive
    Right (Right postLoadImpl) ->
      main' postLoadImpl units dflags0 argv3 flagWarnings
    _ -> pure ()
