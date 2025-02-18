module Test1 where

import Control.Concurrent (MVar)
import Data.Foldable (for_)
import Internal.Args (Args (..))
import Internal.Cache (Cache, emptyCache)
import Internal.Compile (compile)
import Internal.Log (dbgs, newLog, dbg)
import Internal.Session (Env (Env, args, cache, log), withGhc)
import Prelude hiding (log)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv)
import System.FilePath ((</>), dropExtension, takeFileName)
import System.IO.Temp (withSystemTempDirectory)
import Data.List.Extra (replace)

data Conf =
  Conf {
    tmp :: FilePath,
    cache :: MVar Cache,
    args0 :: Args
  }

sanitize :: FilePath -> [String] -> String
sanitize tmp =
  replace tmp "$tmp"  . unwords

one :: Conf -> FilePath -> IO ()
one Conf {..} src = do
  log <- newLog True
  let env = Env {log, cache, args}
  dbg (sanitize tmp fileOptions)
  result <- withGhc env compile
  dbgs result
  pure ()
  where
    args =
      args0 {
        ghcOptions = args0.ghcOptions ++ fileOptions
      }

    fileOptions =
      [
        "-o",
        tmp </> "out" </> (modName ++ ".dyn_o"),
        "-ohi",
        tmp </> "out" </> (modName ++ ".dyn_hi"),
        "-dynohi",
        tmp </> "out" </> (modName ++ ".dyn_hi"),
        src
      ]

    modName = dropExtension (takeFileName src)

dep1Content :: String
dep1Content =
  "module Dep1 where\ndep1 :: Int\ndep1 = 5"

mainContent :: String
mainContent =
  "module Main where\nimport Dep1\nmain :: IO ()\nmain = putStrLn (show dep1)"

baseArgs :: FilePath -> FilePath -> Args
baseArgs ghc_dir tmp =
  Args {
    topdir = Just (ghc_dir </> "lib/ghc-9.10.1/lib"),
    workerTargetId = Just "test",
    env = mempty,
    binPath = [],
    tempDir = Just (tmp </> "tmp"),
    ghcPath = Nothing,
    ghcOptions = (artifactDir =<< ["o", "hie", "dump", "stub"]) ++ [
      "-i" ++ (tmp </> "out"),
      "-dynamic",
      "-fPIC",
      "-osuf",
      "dyn_o",
      "-hisuf",
      "dyn_hi",
      "-this-unit-id",
      "test-1"
    ]
  }
  where
    artifactDir a = ["-" ++ a ++ "dir", tmp </> "out"]

test1 :: IO ()
test1 =
  withSystemTempDirectory "buck-worker-test" \ tmp -> do
    for_ @[] ["src", "tmp", "out"] \ dir ->
      createDirectoryIfMissing False (tmp </> dir)
    let
      pathDep1 = tmp </> "src/Dep1.hs"
      pathMain = tmp </> "src/Main.hs"
    writeFile pathDep1 dep1Content
    writeFile pathMain mainContent
    cache <- emptyCache False
    ghc_dir <- getEnv "ghc_dir"
    let conf = Conf {tmp, cache, args0 = baseArgs ghc_dir tmp}
    dbg (sanitize tmp conf.args0.ghcOptions)
    one conf pathDep1
    one conf pathMain
