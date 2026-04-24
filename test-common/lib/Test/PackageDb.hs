module Test.PackageDb where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Paths (ghc_pkg)
import Prelude hiding (log)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))
import System.Process.Typed (proc, runProcess_)

-- | Config for a single test module.
data ModuleSpec =
  ModuleSpec {
    -- | Module name.
    name :: String,

    -- | The module's source code.
    content :: ByteString,

    boot :: Bool
  }
  deriving stock (Eq, Show)

moduleSpec :: String -> [ByteString] -> ModuleSpec
moduleSpec name content =
  ModuleSpec {
    name,
    content = ByteString.unlines content,
    boot = False
  }

toBoot :: ModuleSpec -> ModuleSpec
toBoot spec =
  spec {boot = True}

-- | Config for a single test unit.
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

-- | A package DB config file for a test unit.
dbConf ::
  FilePath ->
  String ->
  NonEmpty String ->
  String
dbConf srcDir unit modules =
  unlines [
    "name: " ++ unit,
    "version: 1.0",
    "id: " ++ unit,
    "key: " ++ unit,
    "import-dirs: " ++ srcDir,
    "exposed: True",
    "exposed-modules: " ++ mconcat (intersperse ", " exposed)
  ]
  where
    exposed = toList modules

-- | Write a fresh package DB without a library to the specified directory, using @ghc-pkg@ from @ghc-paths@.
createDb :: String -> String -> IO String
createDb dir confFile = do
  createDirectoryIfMissing False db
  runProcess_ (proc ghc_pkg ["-v0", "--package-db", db, "recache"])
  runProcess_ (proc ghc_pkg ["-v0", "--package-db", db, "register", "--force", confFile])
  pure db
  where
    db = dir </> "package.conf.d"

-- | Write a package DB config file and generate the DB itself.
writeDb :: UnitSpec -> FilePath -> String -> IO FilePath
writeDb unit dir db = do
  createDirectoryIfMissing False dir
  writeFile confFile db
  createDb dir confFile
  where
    confFile = dir </> unit.name <.> "conf"

-- | Create a package DB for a set of 'ModuleSpec' and return the path to the DB.
createEmptyHomeUnitDb :: UnitSpec -> FilePath -> NonEmpty String -> IO FilePath
createEmptyHomeUnitDb unit dir modules =
  writeDb unit dir (dbConf dir unit.name modules)
