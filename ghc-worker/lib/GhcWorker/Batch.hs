module GhcWorker.Batch where

import Control.Concurrent.STM (newTVarIO)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import GhcWorker.GhcHandler (LockState (..), dispatch)
import GhcWorker.Instrumentation (hooksNoop)
import Internal.Log (TraceId (..), dbg, newLog)
import Internal.Session (Env (..))
import Internal.State (newStateWith)
import Internal.State.Oneshot (OneshotCacheFeatures (..))
import Internal.State.Stats (logMemStats)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.Environment (getEnv)
import System.FilePath (addExtension, replaceExtension, takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Types.Args (Args (..), emptyArgs)
import qualified Types.BuckArgs as BuckArgs
import Types.BuckArgs (Mode (..), emptyBuckArgs)
import Types.GhcHandler (WorkerMode (..))

batchCompileWith ::
  FilePath ->
  Env ->
  [String] ->
  [FilePath] ->
  IO ()
batchCompileWith tmp env extraOptions paths = do
  lock <- newTVarIO LockStart
  createDirectoryIfMissing True out
  _ <- dispatch lock WorkerMakeMode hooksNoop (withOptions (metadataOptions ++ extraOptions ++ paths)) buckArgs {BuckArgs.mode = Just ModeMetadata}
  for_ paths \ path -> do
    let name = takeFileName path
    dbg name
    dispatch lock WorkerMakeMode hooksNoop (withOptions (compileOptions name ++ [path])) buckArgs {BuckArgs.mode = Just ModeCompile, BuckArgs.abiOut = Just (out </> addExtension name "hash")}
  where
    out = tmp </> "out"

    withOptions opts = env {args = env.args {ghcOptions = env.args.ghcOptions ++ opts}}

    compileOptions name =
      [
        "-fwrite-ide-info",
        "-no-link",
        "-i",
        "-hide-all-packages",
        "-dynamic",
        "-fPIC",
        "-osuf", "dyn_o",
        "-hisuf", "dyn_hi",
        "-o", out </> replaceExtension name "dyn_o",
        "-ohi", out </> replaceExtension name "dyn_hi",
        "-odir", out,
        "-hiedir", out,
        "-dumpdir", out,
        "-stubdir", out,
        "-i" ++ out,
        "-package", "base",
        "-package", "template-haskell",
        "-fprefer-byte-code",
        "-fbyte-code-and-object-code"
      ]

    metadataOptions = [
      "-i",
      "-hide-all-packages",
      "-include-pkg-deps",
      "-package", "base",
      "-package", "template-haskell",
      "-dep-json", out </> "depends.json",
      "-dep-makefile", out </> "depends.make",
      "-outputdir", ".",
      "-fprefer-byte-code",
      "-fbyte-code-and-object-code"
      ]

    buckArgs = emptyBuckArgs mempty


batchCompile :: [String] -> IO ()
batchCompile argv =
  withSystemTempDirectory "batch-worker" \ tmp -> do
  logVar <- newLog (Just (TraceId "batch"))
  state <- newStateWith OneshotCacheFeatures {
          loader = False,
          enable = True,
          names = False,
          finder = False,
          eps = False
        }
  ghcDir <- getEnv "ghc_dir"
  libPath <- listDirectory (ghcDir </> "lib") <&> \case
    [d] -> "lib" </> d </> "lib"
    ds -> error ("weird GHC lib dir contains /= 1 entries: " ++ show ds)
  let ghcTemp = tmp </> "ghc-tmp"
      args = (emptyArgs mempty) {
        topdir = Just (ghcDir </> libPath),
        tempDir = Just ghcTemp,
        ghcOptions = [
          "-this-unit-id", "test"
        ]
      }
      env = Env {log = logVar, state, args}
      extraOptions = words =<< take 1 argv
      paths = drop 1 argv
  createDirectoryIfMissing True ghcTemp
  batchCompileWith tmp env extraOptions paths
  logMemStats "final" logVar
