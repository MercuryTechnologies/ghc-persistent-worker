{-# LANGUAGE NumericUnderscores #-}
module Worker where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import qualified GHC
import GHC.Driver.Backend (backendNeedsFullWays)
import GHC.Driver.Config.Diagnostic (initDiagOpts, initPrintConfig)
import GHC.Driver.Config.Logger (initLogFlags)
import GHC.Driver.Monad (Ghc)
import GHC.Driver.Phases (StopPhase (NoStop))
import GHC.Driver.Session (gopt_set, gopt_unset)
import GHC.Main (PostLoadMode (..), main', parseModeFlags, showBanner)
import GHC.Platform.Ways (hostFullWays, wayGeneralFlags, wayUnsetGeneralFlags)
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
  -- args <- getArgs
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
    logMessage (prompt ++ " Got args: " ++ show args)
    --
    compileMain args -- liftIO $ threadDelay 15_000_000
    --
    liftIO $ hPutStrLn hout "AfterGHC"
    liftIO $ hFlush hout

compileMain :: [String] -> Ghc ()
compileMain args = do
  GHC.initGhcMonad Nothing
  let argv2 = map (GHC.mkGeneralLocated "on the commandline") args
  (_mode, units, argv3, flagWarnings) <- liftIO $ parseModeFlags argv2

  dflags0 <- GHC.getSessionDynFlags
  main' (StopBefore NoStop) units dflags0 argv3 flagWarnings
