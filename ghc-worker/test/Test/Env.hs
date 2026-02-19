module Test.Env where

import Data.Functor ((<&>))
import GHC.Data.OsPath (unsafeDecodeUtf)
import System.Directory (listDirectory, removeDirectoryRecursive)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.OsPath (OsPath, unsafeEncodeUtf)
import Test.Data.Env (SessionEnv (..), TestEnv (..))
import Test.Run (mkEnv)
import Test.Tasty (TestTree, withResource)
import Types.Args (Args (..), emptyArgs)

-- | Grab the GHC directory from the environment and store it in the 'Args' of the returned 'TestEnv'.
-- This is the directory that contains the @settings@ file that provides the paths to various GHC components.
mkTestEnv :: OsPath -> IO TestEnv
mkTestEnv rootDir = do
  ghcDir <- getEnv "ghc_dir"
  libPath <- listDirectory (ghcDir </> "lib") <&> \case
    [d] -> "lib" </> d </> "lib"
    ds -> error ("weird GHC lib dir contains /= 1 entries: " ++ show ds)
  pure TestEnv {
    rootDir,
    baseArgs = (emptyArgs []) {topdir = Just (ghcDir </> libPath)}
  }

-- | Create a new environment for a build test run consisting of two builds.
--
-- Create directories for:
-- - Source files, which are written by the test machinery
-- - Temp files, which includes all of GHC's output artifacts as well as its own temp dir
--
-- Create an empty worker state, shared only across tasks within one build.
-- This is discarded and recreated when the second build is started, in 'newResumeSessionEnv'.
newSessionEnv :: TestEnv -> IO SessionEnv
newSessionEnv shared@TestEnv {rootDir} = do
  sourceDir <- unsafeEncodeUtf <$> createTempDirectory rootDirFP "src"
  tempDir <- unsafeEncodeUtf <$> createTempDirectory rootDirFP "tmp"
  (env, _) <- mkEnv
  pure SessionEnv {shared, sourceDir, tempDir, env}
  where
    rootDirFP = unsafeDecodeUtf rootDir

-- | Reuses the previous session's @srcDir@ and @tmpDir@ (preserving written sources and artifacts) but creates a fresh
-- 'Env' with an empty 'WorkerState', simulating a worker restart.
--
-- Some of the artifacts are deleted by @BuildSystem@, representing an action performed by Buck.
newResumeSessionEnv :: SessionEnv -> IO SessionEnv
newResumeSessionEnv prev = do
  (env, _) <- mkEnv
  pure prev {env}

acquireTestEnv :: IO TestEnv
acquireTestEnv = do
  tmpBase <- getCanonicalTemporaryDirectory
  rootDir <- unsafeEncodeUtf <$> createTempDirectory tmpBase "project-build-test"
  mkTestEnv rootDir

releaseTestEnv :: TestEnv -> IO ()
releaseTestEnv env =
  removeDirectoryRecursive (unsafeDecodeUtf env.rootDir)

withTestEnv :: (IO TestEnv -> TestTree) -> TestTree
withTestEnv =
  withResource acquireTestEnv releaseTestEnv
