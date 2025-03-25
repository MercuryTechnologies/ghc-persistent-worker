module TestSetup where

import Control.Concurrent (MVar)
import Data.Foldable (for_, toList)
import Data.Functor ((<&>))
import Data.List.Extra (nubOrd)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Unit (UnitId, stringToUnitId, unitIdString)
import GHC.Utils.Outputable (showPprUnsafe)
import Internal.Args (Args (..))
import Internal.Cache (Cache (..), CacheFeatures (..), emptyCacheWith)
import Internal.Log (dbg)
import Prelude hiding (log)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.Environment (getEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (proc, runProcess_)

data Conf =
  Conf {
    tmp :: FilePath,
    cache :: MVar Cache,
    args0 :: Args,
    ghcDir :: FilePath,
    libPath :: FilePath
  }

data UnitMod =
  UnitMod {
    name :: String,
    src :: FilePath,
    unit :: String,
    deps :: [String],
    content :: String
  }
  deriving stock (Eq, Show)

data UnitConf =
  UnitConf {
    uid :: UnitId,
    db :: FilePath,
    mods :: NonEmpty UnitMod,
    externalDeps :: [UnitId]
  }
  deriving stock (Eq)

baseArgs :: FilePath -> FilePath -> Args
baseArgs topdir tmp =
  Args {
    topdir = Just topdir,
    workerTargetId = Just "test",
    env = mempty,
    binPath = [],
    tempDir = Just (tmp </> "tmp"),
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

createDbUnitMod :: Conf -> Set String -> NonEmpty UnitMod -> IO UnitConf
createDbUnitMod conf homeUnits mods@(mod0 :| _) = do
  let uid = stringToUnitId mod0.unit
  writeFile confFile (dbConf dir mod0.unit (toList mods))
  db <- createDb conf dir confFile
  pure UnitConf {..}
  where
    externalDeps = stringToUnitId <$> filter (not . flip Set.member homeUnits) (nubOrd (concat ((.deps) <$> mods)))
    confFile = dir </> (mod0.unit ++ ".conf")
    dir = takeDirectory mod0.src

createDbExternal :: Conf -> UnitId -> String -> IO String
createDbExternal conf unit storePath = do
  name <- listDirectory pcd <&> \case
      [pconf] -> pconf
      ds -> error ("weird package.conf.d dir for " ++ showPprUnsafe unit ++ " contains /= 1 entries: " ++ show ds)
  let confFile = pcd </> name
  createDirectoryIfMissing False dir
  createDb conf dir confFile
  where
    dir = conf.tmp </> unitIdString unit
    pcd = storePath </> conf.libPath </> "package.conf.d"

withProject ::
  (Conf -> IO [UnitMod]) ->
  (Conf -> [UnitConf] -> [UnitMod] -> IO a) ->
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
    let unitMods = NonEmpty.groupAllWith (.unit) targets
        homeUnits = Set.fromList [unit | UnitMod {unit} :| _ <- unitMods]
    units <- traverse (createDbUnitMod conf homeUnits) unitMods
    use conf units targets
