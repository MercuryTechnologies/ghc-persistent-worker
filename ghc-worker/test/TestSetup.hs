module TestSetup where

import Control.Concurrent (MVar)
import Data.Foldable (for_, toList)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Traversable (for)
import GHC.Unit (UnitId, stringToUnitId, unitIdString)
import Internal.Log (dbg)
import Internal.State (WorkerState (..), newStateWith)
import Internal.State.Oneshot (OneshotCacheFeatures (..))
import Prelude hiding (log)
import System.Directory (createDirectoryIfMissing, listDirectory, withCurrentDirectory)
import System.Environment (getEnv)
import System.FilePath ((<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (proc, runProcess_)
import Types.Args (Args (..), TargetId (..))

-- | Global configuration for a worker compilation test.
data Conf =
  Conf {
    -- | Root directory of the test in @/tmp@.
    tmp :: FilePath,

    -- | The worker state.
    state :: MVar WorkerState,

    -- | The base cli args used for all modules.
    args0 :: Args,

    -- | The directory containing the GHC(-pkg) binaries at @bin/@ and the settings (topdir) at @lib/ghc-*/lib/@.
    ghcDir :: FilePath,

    -- | The relative path to the topdir in 'ghcDir', with the version number spelled out.
    libPath :: FilePath
  }

-- | Config for a single test module.
data ModuleSpec =
  ModuleSpec {
    -- | Module name.
    name :: String,

    -- | The module's source code.
    content :: String
  }
  deriving stock (Eq, Show)

-- | Config for a single test home unit.
data UnitSpec =
  UnitSpec {
    -- | Unit ID.
    name :: String,

    -- | Names of home units on which this unit depends.
    deps :: [String],

    -- | The modules belonging to this unit.
    modules :: NonEmpty ModuleSpec
  }
  deriving stock (Eq, Show)

-- | Generated data for a test module.
data Module =
  Module {
    -- | Module name.
    name :: String,

    -- | Path to the source file.
    src :: FilePath,

    -- | Home unit to which this module belongs.
    unit :: String
  }
  deriving stock (Eq, Show)

-- | Generated data for a test unit.
data Unit =
  Unit {
    -- | Unit ID.
    uid :: UnitId,

    -- | Unit ID.
    name :: String,

    -- | Root source directory of this unit.
    dir :: FilePath,

    -- | Names of home units on which this unit depends.
    deps :: [String],

    -- | Path to the dummy package DB created for the metadata step, analogous to what's created by Buck.
    db :: FilePath,

    -- | The modules belonging to this unit.
    modules :: NonEmpty Module
  }
  deriving stock (Eq)

instance Show Unit where
  showsPrec d Unit {..} =
    showParen (d > 5) (
      showString "Unit { uid = "
      .
      showsPrec 5 (unitIdString uid)
      .
      showString ", name = "
      .
      showsPrec 5 name
      .
      showString ", dir = "
      .
      showsPrec 5 dir
      .
      showString ", db = "
      .
      showsPrec 5 db
      .
      showString ", modules = "
      .
      showsPrec 5 modules
      .
      showString " }"
    )

-- | General CLI args used by each module job.
baseArgs :: FilePath -> FilePath -> Args
baseArgs topdir tmp =
  Args {
    topdir = Just topdir,
    workerTargetId = Just (TargetId "test"),
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
    ],
    cachedDeps = Nothing
  }
  where
    artifactDir a = ["-" ++ a ++ "dir", tmp </> "out"]

-- | A package DB config file for the given unit.
dbConf :: FilePath -> String -> NonEmpty Module -> String
dbConf srcDir unit modules =
  unlines [
    "name: " ++ unit,
    "version: 1.0",
    "id: " ++ unit,
    "key: " ++ unit,
    "import-dirs: " ++ srcDir,
    "exposed: True",
    "exposed-modules:" ++ unwords exposed
  ]
  where
    exposed = [name | Module {name} <- toList modules]

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

-- | Create a package DB for a set of 'ModuleSpec' and assemble everything into a 'Unit'.
createDbForUnit :: Conf -> UnitSpec -> FilePath -> NonEmpty Module -> IO FilePath
createDbForUnit conf unit dir modules = do
  writeFile confFile (dbConf dir unit.name modules)
  createDb conf dir confFile
  where
    confFile = dir </> unit.name <.> "conf"

-- | Set up an environment with dummy package DBs for the set of modules returned by the first argument, then run the
-- second argument with the resulting unit configurations.
withProject ::
  (Conf -> IO (NonEmpty UnitSpec)) ->
  (Conf -> NonEmpty Unit -> IO a) ->
  IO a
withProject mkTargets use =
  withSystemTempDirectory "buck-worker-test" \ tmp -> do
    withCurrentDirectory tmp do
      for_ @[] ["src", "tmp", "out"] \ dir ->
        createDirectoryIfMissing False (tmp </> dir)
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
      let topdir = ghcDir </> libPath
          conf = Conf {tmp, state, args0 = baseArgs topdir tmp, ..}
      targets <- mkTargets conf
      units <- for targets \ unit -> do
        let dir = tmp </> "src" </> unit.name
        createDirectoryIfMissing False dir
        modules <- for unit.modules \ ModuleSpec {name, content} -> do
          let src = dir </> name <.> "hs"
          writeFile src content
          pure Module {unit = unit.name, ..}
        db <- createDbForUnit conf unit dir modules
        pure Unit {
          uid = stringToUnitId unit.name,
          name = unit.name,
          deps = unit.deps,
          dir,
          db,
          modules
        }
      use conf units
