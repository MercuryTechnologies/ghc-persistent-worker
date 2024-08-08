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
import GHC.Driver.Monad (Ghc)
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
import Message (Msg (..), recvMsg, sendMsg, unwrapMsg, wrapMsg)
import Network.Socket (Socket)
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
  forever $ do
    s <- liftIO $ hGetLine hin
    let args :: [String] = read s
    logMessage (prompt ++ " Got args: " ++ intercalate " " args)
    --
    compileMain args
    --
    liftIO $ hPutStrLn hout "AfterGHC"
    liftIO $ hFlush hout
    liftIO $ hPutStrLn stdout "*D*E*L*I*M*I*T*E*D*"
    liftIO $ hFlush stdout


compileMain :: [String] -> Ghc ()
compileMain args = do
  GHC.initGhcMonad Nothing
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
