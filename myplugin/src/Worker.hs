{-# LANGUAGE NumericUnderscores #-}
module Worker where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import qualified GHC
import GHC.Driver.Backend (backendNeedsFullWays)
import GHC.Driver.Config.Logger (initLogFlags)
import GHC.Driver.Monad (Ghc)
import GHC.Driver.Session (gopt_set, gopt_unset)
import GHC.Platform.Ways (hostFullWays, wayGeneralFlags, wayUnsetGeneralFlags)
import GHC.Utils.Logger (setLogFlags)
import Message (Msg (..), recvMsg, sendMsg, unwrapMsg, wrapMsg)
import Network.Socket (Socket)
import System.Environment (getArgs)
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
    liftIO $ hPutStrLn hout "ABCDE"
    liftIO $ hFlush hout

compileMain :: [String] -> Ghc ()
compileMain args = do
  let argv2 = map (GHC.mkGeneralLocated "on the commandline") args
{-      parseModeFlags :: [GHC.Located String] -> IO (Mode, [String], [GHC.Located String], [GHC.Warn])
      parseModeFlags args = do
        ((leftover, errs1, warns), (mModeFlag, units, errs2, flags')) <-
              processCmdLineP mode_flags (Nothing, [], [], []) args
        let mode = case mModeFlag of
                   Nothing     -> undefined
                   Just (m, _) -> m

  (_mode, _units, argv3, flagWarnings) <- parseModeFlags argv2 -}
  let args' = argv2

  dflags0 <- GHC.getSessionDynFlags
  let dflt_backend = GHC.backend dflags0
      (mode, bcknd, link) = (GHC.OneShot, dflt_backend, GHC.LinkBinary)
  let dflags1 = dflags0{ GHC.ghcMode   = mode,
                         GHC.backend   = bcknd,
                         GHC.ghcLink   = link,
                         GHC.verbosity = 1
                        }
      dflags2 = dflags1
  logger1 <- GHC.getLogger
  let logger2 = setLogFlags logger1 (initLogFlags dflags2)
  (dflags3, fileish_args, dynamicFlagWarnings) <-
    GHC.parseDynamicFlags logger2 dflags2 args'

  let dflags4 = if backendNeedsFullWays bcknd &&
                   not (GHC.gopt GHC.Opt_ExternalInterpreter dflags3)
                then
                    let platform = GHC.targetPlatform dflags3
                        dflags3a = dflags3 { GHC.targetWays_ = hostFullWays }
                        dflags3b = foldl gopt_set dflags3a
                                 $ concatMap (wayGeneralFlags platform)
                                             hostFullWays
                        dflags3c = foldl gopt_unset dflags3b
                                 $ concatMap (wayUnsetGeneralFlags platform)
                                             hostFullWays
                    in dflags3c
                else
                    dflags3

  let logger4 = setLogFlags logger2 (initLogFlags dflags4)
  pure ()
{-
  GHC.prettyPrintGhcErrors logger4 $ do

    let flagWarnings' = flagWarnings ++ dynamicFlagWarnings
    pure () -}
{-
    handleSourceError (\e -> do
         GHC.printException e
         liftIO $ exitWith (ExitFailure 1)) $ do
           liftIO $ handleFlagWarnings logger4 (initPrintConfig dflags4) (initDiagOpts dflags4) flagWarnings'

    liftIO $ showBanner postLoadMode dflags4

    let (dflags5, srcs, objs) = parseTargetFiles dflags4 (map unLoc fileish_args)

    -- we've finished manipulating the DynFlags, update the session
    _ <- GHC.setSessionDynFlags dflags5
    dflags6 <- GHC.getSessionDynFlags

    -- Must do this before loading plugins
    liftIO $ initUniqSupply (initialUnique dflags6) (uniqueIncrement dflags6)

    -- Initialise plugins here because the plugin author might already expect this
    -- subsequent call to `getLogger` to be affected by a plugin.
    initializeSessionPlugins
    hsc_env <- getSession
    logger <- getLogger


          ---------------- Display configuration -----------
    case verbosity dflags6 of
      v | v == 4 -> liftIO $ dumpUnitsSimple hsc_env
        | v >= 5 -> liftIO $ dumpUnits       hsc_env
        | otherwise -> return ()

          ---------------- Final sanity checking -----------
    liftIO $ checkOptions postLoadMode dflags6 srcs objs units
    hsc_env <- getSession
-}
