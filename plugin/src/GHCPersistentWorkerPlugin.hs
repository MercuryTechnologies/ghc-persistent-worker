{-# LANGUAGE NumericUnderscores #-}
module GHCPersistentWorkerPlugin (frontendPlugin) where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime)
import GHC.Driver.Env (HscEnv (hsc_NC, hsc_interp, hsc_unit_env))
import GHC.Driver.Monad (Ghc, withSession, modifySession)
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
import qualified GHC
import System.Directory (setCurrentDirectory)
import System.Environment (setEnv)
import System.IO
  ( BufferMode (..),
    hFlush,
    hGetLine,
    hPutStrLn,
    hSetBuffering,
    stdin,
    stderr,
    stdout,
  )
import Util (openFileAfterCheck, openPipeRead, openPipeWrite)


frontendPlugin :: FrontendPlugin
frontendPlugin = defaultFrontendPlugin
  {
    frontend = \flags _args -> workerMain flags
  }

logMessage :: String -> Ghc ()
logMessage = liftIO . hPutStrLn stderr

workerMain :: [String] -> Ghc ()
workerMain flags = do
  liftIO $ do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

  let n :: Int = read (flags !! 0)
      prompt = "[Worker:" ++ show n ++ "]"
  let (hin, hout) = (stdin, stdout)
  logMessage (prompt ++ " Started")
  GHC.initGhcMonad Nothing
  forever $ do
    s <- liftIO $ hGetLine hin
    let (env, args) :: ([(String, String)], [String]) = read s
    for_ (lookup "PWD" env) $ \pwd -> liftIO $
      setCurrentDirectory pwd
    for_ env $ \(var, val) -> liftIO $
      setEnv var val
    logMessage (prompt ++ " Got args: " ++ intercalate " " args)
    --
    liftIO $ do
      mapM_ (\_ -> hPutStrLn stderr "=================================") [1..5]
      time <- getCurrentTime
      hPutStrLn stderr $ "worker: " ++ (show n)
      hPutStrLn stderr (show time)
      mapM_ (\_ -> hPutStrLn stderr "=================================") [1..5]
    --
    compileMain args
    --
    liftIO $ do
      mapM_ (\_ -> hPutStrLn stderr "|||||||||||||||||||||||||||||||||") [1..5]
      time <- getCurrentTime
      hPutStrLn stderr $ "worker: " ++ (show n)
      hPutStrLn stderr (show time)
      mapM_ (\_ -> hPutStrLn stderr "|||||||||||||||||||||||||||||||||") [1..5]
    --
    -- TODO: will have more useful info
    let result = "DUMMY RESULT"
    --
    liftIO $ hPutStrLn hout "*S*T*D*O*U*T*"
    liftIO $ hFlush hout
    liftIO $ hPutStrLn hout result
    liftIO $ hPutStrLn hout "*R*E*S*U*L*T*"
    liftIO $ hFlush hout
    liftIO $ hPutStrLn hout "*D*E*L*I*M*I*T*E*D*"
    liftIO $ hFlush hout
    --

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
