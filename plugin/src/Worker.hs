{-# LANGUAGE NumericUnderscores #-}
module Worker where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import qualified GHC
import GHC.Driver.Backend (backendNeedsFullWays)
import GHC.Driver.Config.Diagnostic (initDiagOpts, initPrintConfig)
import GHC.Driver.Config.Logger (initLogFlags)
import GHC.Driver.Env (HscEnv (hsc_NC, hsc_interp, hsc_unit_env))
import GHC.Driver.Main (initHscEnv)
import GHC.Driver.Monad (Ghc, withSession, modifySession)
import GHC.Driver.Phases (StopPhase (NoStop))
import GHC.Driver.Session (gopt_set, gopt_unset)
import GHC.Main
  ( PostLoadMode (..),
    PreStartupMode (..),
    main',
    parseModeFlags,
    showBanner,
    showSupportedExtensions,
    showVersion,
    showOptions,
  )
import GHC.Platform.Ways (hostFullWays, wayGeneralFlags, wayUnsetGeneralFlags)
import GHC.Settings.Config (cProjectVersion)
import GHC.Utils.Logger (setLogFlags)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO
  ( BufferMode (..),
    IOMode (..),
    hFlush,
    hGetLine,
    hPutStrLn,
    hSetBuffering,
    stderr,
    stdout,
    withFile,
  )
import System.Posix.IO
  ( OpenFileFlags (nonBlock),
    OpenMode (ReadOnly, WriteOnly),
    defaultFileFlags,
    fdToHandle,
    openFd,
  )
import Util (openFileAfterCheck, openPipeRead, openPipeWrite)

import GHC.Driver.Ppr (showSDocUnsafe)
import GHC.Linker.Loader (showLoaderState)
import Data.Time.Clock (getCurrentTime)

--
import Control.Concurrent (readMVar)
import Data.Foldable (toList)
import GHC.Driver.Ppr (showPprUnsafe)
import GHC.Linker.Types (Loader (..), LoaderState (..), LinkerEnv (..))
import GHC.Runtime.Interpreter.Types (interpLoader)
import GHC.Types.Unique.FM (NonDetUniqFM (..))

logMessage :: String -> Ghc ()
logMessage = liftIO . hPutStrLn stderr

workerMain :: [String] -> Ghc ()
workerMain flags = do
  liftIO $ do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

  let n :: Int = read (flags !! 0)
      infile = flags !! 1
      outfile = flags !! 2
      prompt = "[Worker:" ++ show n ++ "]"
  hin <- liftIO $ openFileAfterCheck infile (True, False) openPipeRead
  hout <- liftIO $ openFileAfterCheck outfile (False, True) openPipeWrite
  logMessage (prompt ++ " Started")
  GHC.initGhcMonad Nothing
  forever $ do
    s <- liftIO $ hGetLine hin
    let args :: [String] = read s
    logMessage (prompt ++ " Got args: " ++ intercalate " " args)
    --
    liftIO $ do
      mapM_ (\_ -> hPutStrLn stderr "=================================") [1..5]
      time <- getCurrentTime
      hPutStrLn stderr (show time)
      mapM_ (\_ -> hPutStrLn stderr "=================================") [1..5]
    --
    compileMain args
    --
    liftIO $ do
      mapM_ (\_ -> hPutStrLn stderr "|||||||||||||||||||||||||||||||||") [1..5]
      time <- getCurrentTime
      hPutStrLn stderr (show time)
      mapM_ (\_ -> hPutStrLn stderr "|||||||||||||||||||||||||||||||||") [1..5]
    --
    liftIO $ hPutStrLn hout "AfterGHC"
    liftIO $ hFlush hout
    liftIO $ hPutStrLn stdout "*D*E*L*I*M*I*T*E*D*"
    liftIO $ hFlush stdout
    --

    (minterp, unit_env, nc) <-
      withSession $ \env ->
        pure $ (hsc_interp env, hsc_unit_env env, hsc_NC env)
    GHC.initGhcMonad Nothing
    modifySession $ \env ->
      env
        { hsc_interp = minterp,
          hsc_NC = nc
        }

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
