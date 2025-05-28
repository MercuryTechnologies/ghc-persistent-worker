module TestSetup where

import Control.Concurrent (MVar)
import Data.Foldable (for_, toList)
import Data.Functor ((<&>))
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Unit (UnitId, stringToUnitId)
import Internal.Cache (Cache (..), CacheFeatures (..), emptyCacheWith)
import Internal.Log (dbg)
import Prelude hiding (log)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.Environment (getEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (proc, runProcess_)
import Types.Args (Args (..))

-- | Global configuration for a worker compilation test.
data Conf =
  Conf {
    -- | Root directory of the test in @/tmp@.
    tmp :: FilePath,

    -- | The worker cache.
    cache :: MVar Cache,

    -- | The base cli args used for all modules.
    args0 :: Args,

    -- | The directory containing the GHC(-pkg) binaries at @bin/@ and the settings (topdir) at @lib/ghc-*/lib/@.
    ghcDir :: FilePath,

    -- | The relative path to the topdir in 'ghcDir', with the version number spelled out.
    libPath :: FilePath
  }

-- | Config for a single test module.
data UnitMod =
  UnitMod {
    -- | Module name.
    name :: String,

    -- | Path to the source file.
    src :: FilePath,

    -- | Home unit to which this module belongs.
    unit :: String,

    -- | Names of home units on which this module depends.
    deps :: [String],

    -- | The module's source code.
    content :: String
  }
  deriving stock (Eq, Show)

-- | Config for a single test home unit.
data UnitConf =
  UnitConf {
    -- | Unit ID.
    uid :: UnitId,

    -- | Path to the dummy package DB created for the metadata step, analogous to what's created by Buck.
    db :: FilePath,

    -- | The modules belonging to this unit.
    mods :: NonEmpty UnitMod
  }
  deriving stock (Eq)

-- | General CLI args used by each module job.
baseArgs :: FilePath -> FilePath -> Args
baseArgs topdir tmp =
  Args {
    topdir = Just topdir,
    workerTargetId = Just "test",
    env = mempty,
    binPath = [],
    tempDir = Nothing,
    ghcPath = Nothing,
    ghcOptions = (artifactDir =<< ["o", "hie", "dump"]) ++ [
      "-fwrite-ide-info",
      "-no-link",
      "-dynamic",
      -- "-fwrite-if-simplified-core",
      "-fbyte-code-and-object-code",
      "-fprefer-byte-code",
      -- "-fpackage-db-byte-code",
      -- "-shared",
      "-fPIC",
      "-osuf",
      "dyn_o",
      "-hisuf",
      "dyn_hi",
      "-package",
      "base"
      -- , "-v"
      -- , "-ddump-if-trace"
    ]
  }
  where
    artifactDir a = ["-" ++ a ++ "dir", tmp </> "out"]

-- | A package DB config file for the given unit.
dbConf :: String -> String -> [UnitMod] -> String
dbConf srcDir unit mods =
  unlines $ [
    "name: " ++ unit,
    "version: 1.0",
    "id: " ++ unit,
    "key: " ++ unit,
    "import-dirs: " ++ srcDir,
    "exposed: True",
    "exposed-modules:"
  ] ++
  exposed
  where
    exposed = [name | UnitMod {name} <- mods]

-- | Write a fresh package DB without a library to the specified directory, using @ghc-pkg@ from the directory in
-- 'Conf'.
createDb :: Conf -> String -> String -> IO String
createDb conf dir confFile = do
  dbg ("create db for " ++ confFile ++ " at " ++ db)
  createDirectoryIfMissing False db
  runProcess_ (proc ghcPkg ["--package-db", db, "recache"])
  runProcess_ (proc ghcPkg ["--package-db", db, "register", "--force", confFile])
  pure db
  where
    db = dir </> "package.conf.d"
    ghcPkg = conf.ghcDir </> "bin/ghc-pkg"

-- | Create a package DB for a set of 'UnitMod' and assemble everything into a 'UnitConf'.
createDbUnitMod :: Conf -> NonEmpty UnitMod -> IO UnitConf
createDbUnitMod conf mods@(mod0 :| _) = do
  let uid = stringToUnitId mod0.unit
  writeFile confFile (dbConf dir mod0.unit (toList mods))
  db <- createDb conf dir confFile
  pure UnitConf {..}
  where
    confFile = dir </> (mod0.unit ++ ".conf")
    dir = takeDirectory mod0.src

-- | Set up an environment with dummy package DBs for the set of modules returned by the first argument, then run the
-- second argument with the resulting unit configurations.
withProject ::
  (Conf -> IO (NonEmpty UnitMod)) ->
  (Conf -> [UnitConf] -> NonEmpty UnitMod -> IO a) ->
  IO a
withProject mkTargets use =
  withSystemTempDirectory "buck-worker-test" \ tmp -> do
    for_ @[] ["src", "tmp", "out"] \ dir ->
      createDirectoryIfMissing False (tmp </> dir)
    cache <- emptyCacheWith CacheFeatures {
      hpt = True,
      loader = False,
      enable = True,
      names = False,
      finder = True,
      eps = False
    }
    ghcDir <- getEnv "ghc_dir"
    libPath <- listDirectory (ghcDir </> "lib") <&> \case
      [d] -> "lib" </> d </> "lib"
      ds -> error ("weird GHC lib dir contains /= 1 entries: " ++ show ds)
    let topdir = ghcDir </> libPath
        conf = Conf {tmp, cache, args0 = baseArgs topdir tmp, ..}
    targets <- mkTargets conf
    for_ targets \ UnitMod {src, content} -> do
      createDirectoryIfMissing False (takeDirectory src)
      writeFile src content
    let unitMods = NonEmpty.groupAllWith (.unit) (toList targets)
    units <- traverse (createDbUnitMod conf) unitMods
    use conf units targets
