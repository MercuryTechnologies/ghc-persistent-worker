{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
-- | Infrastructure for creating external dependency packages in tests.
--
-- Each external dep is a minimal package containing a single module that exports an @Int@ value.
-- The packages are compiled by GHC and registered in a per-package DB, allowing home-unit modules to import from
-- them via @-package@ and @-package-db@ flags.
--
-- When the environment variable @resource_test_ext_deps@ is set (pointing to a directory of prebuilt packages from
-- Nix), packages are symlinked from the Nix store instead of compiled at runtime.
module Test.ExtDep where

import qualified Data.ByteString.Char8 as ByteString
import Data.Foldable (toList)
import Data.Set (Set)
import Data.Traversable (for)
import GHC.Paths (ghc, ghc_pkg)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Environment (lookupEnv)
import System.FilePath ((<.>), (</>))
import System.IO (hPutStrLn, stderr)
import System.OsPath (OsPath)
import System.Process.Typed (proc, runProcess_)
import Test.Path (extDepModuleName, extDepName, extDepValueName, fp)

-- | Write the source file for an external dependency's module.
--
-- The module exports a single @Int@ value that can be imported by home-unit modules.
writeExtDepSource :: FilePath -> Int -> IO ()
writeExtDepSource dir i = do
  createDirectoryIfMissing True dir
  ByteString.writeFile (dir </> extDepModuleName i <.> "hs") content
  where
    content = ByteString.pack $ unlines [
      "module " ++ extDepModuleName i ++ " where",
      "",
      extDepValueName i ++ " :: Int",
      extDepValueName i ++ " = " ++ show (i + 1)
      ]

-- | Compile an external dependency's source file and produce @.dyn_hi@, @.dyn_o@, and @.so@ files.
--
-- Static @.hi@\/@.o@ are unnecessary: downsweep runs in @MkDepend@ mode, which enables
-- @finder_bypassHiFileCheck@ and skips the file existence check for single-import-dir packages.
-- Compilation uses the @.dyn_hi@ interface files since the session is @-dynamic@.
--
-- The shared library name includes the GHC version suffix to match what GHC's @findHSDll@ in
-- @GHC.Linker.Loader.locateLib@ searches for when loading packages for TH evaluation.
compileExtDep :: FilePath -> Int -> IO ()
compileExtDep dir i = do
  runProcess_ (proc ghc dynArgs)
  runProcess_ (proc ghc linkArgs)
  where
    name = extDepName i
    modFile = dir </> extDepModuleName i <.> "hs"
    versionedSo = "libHS" ++ name ++ "-ghc" ++ __GLASGOW_HASKELL_FULL_VERSION__ <.> "so"

    commonArgs = [
      "-v0",
      "-this-unit-id", name,
      "-hide-all-packages",
      "-package", "base",
      "-odir", dir,
      "-hidir", dir,
      modFile
      ]

    -- | Dynamic interface and object files, used by @-dynamic@ sessions for compilation and TH.
    dynArgs = commonArgs ++ ["-dynamic", "-osuf", "dyn_o", "-hisuf", "dyn_hi"]

    -- | Shared library from .dyn_o for TH evaluation.
    -- The name must include the GHC version suffix to match what GHC's linker expects
    -- (see @findHSDll@ / @hs_dyn_lib_name@ in @GHC.Linker.Loader.locateLib@).
    linkArgs = [
      "-v0", "-this-unit-id", name, "-dynamic", "-shared",
      "-o", dir </> versionedSo,
      dir </> extDepModuleName i <.> "dyn_o"
      ]

-- | Package DB configuration for an external dependency.
extDepDbConf :: FilePath -> Int -> String
extDepDbConf dir i =
  unlines [
    "name: " ++ name,
    "version: 1.0",
    "id: " ++ name,
    "key: " ++ name,
    "import-dirs: " ++ dir,
    "library-dirs: " ++ dir,
    "dynamic-library-dirs: " ++ dir,
    "hs-libraries: HS" ++ name,
    "exposed: True",
    "exposed-modules: " ++ extDepModuleName i
  ]
  where
    name = extDepName i

-- | Create per-package DBs for all external dependency packages, matching the Buck model where each
-- external dep has its own @package.conf.d@ and the transitive closure is passed as separate @-package-db@ flags.
--
-- When @resource_test_ext_deps@ is set, symlinks prebuilt packages from the Nix store.
-- Otherwise, compiles packages from scratch at runtime.
-- Returns a list of package DB paths (one per ext dep).
createExtDepPackageDbs :: OsPath -> Set Int -> IO [FilePath]
createExtDepPackageDbs _ extDeps
  | null extDeps = pure []
createExtDepPackageDbs tempDir extDeps =
  lookupEnv "resource_test_ext_deps" >>= \case
    Just dir -> usePrebuiltExtDeps dir
    Nothing -> do
      hPutStrLn stderr "$resource_test_ext_deps is unset. Compiling external deps at runtime."
      compileExtDeps
  where
    fpTempDir = fp tempDir

    usePrebuiltExtDeps prebuiltDir =
      fmap concat $ for (toList extDeps) \ i -> do
        let name = extDepName i
            srcDir = prebuiltDir </> name
        exists <- doesDirectoryExist srcDir
        if exists
          then pure [srcDir </> "package.conf.d"]
          else compileSingleExtDep i

    compileExtDeps =
      fmap concat $ for (toList extDeps) compileSingleExtDep

    compileSingleExtDep i = do
      let name = extDepName i
          pkgDir = fpTempDir </> "extdeps" </> name
          db = pkgDir </> "package.conf.d"
      writeExtDepSource pkgDir i
      compileExtDep pkgDir i
      createDirectoryIfMissing True db
      runProcess_ (proc ghc_pkg ["-v0", "--package-db", db, "recache"])
      let confFile = pkgDir </> name <.> "conf"
      writeFile confFile (extDepDbConf pkgDir i)
      runProcess_ (proc ghc_pkg ["-v0", "--package-db", db, "register", "--force", confFile])
      pure [db]
