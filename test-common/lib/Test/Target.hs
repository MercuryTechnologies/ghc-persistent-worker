{-# LANGUAGE PatternSynonyms #-}

module Test.Target where

import qualified Data.ByteString as ByteString
import Data.List.NonEmpty (NonEmpty)
import Data.Time (UTCTime (..), pattern YearMonthDay)
import GHC (Target (..), TargetId (..))
import GHC.Data.StringBuffer (stringBufferFromByteString)
import GHC.Unit (UnitId, stringToUnitId, unitIdString)
import Prelude hiding (log)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))
import Test.PackageDb (ModuleSpec (..), UnitSpec (..))

-- | GHC args used by metadata tests.
-- If a package DB is specified for a unit in the right pair element, add @-package-db@, for use with oneshot simulation
-- tests.
ghcOptions :: UnitId -> [(UnitId, Maybe FilePath)] -> [String]
ghcOptions unit deps =
  [
    "-hide-all-packages",
    "-package", "base",
    "-this-unit-id", unitIdString unit
  ] ++ concat (depArgs <$> deps)
  where
    depArgs (name, db) = ["-package-id", unitIdString name] ++ foldMap dbArg db

    dbArg path = ["-package-db", path]

-- | A GHC target that provides the file contents in memory.
-- This allows running tests without having to write files for modules, which GHC only supports very sporadically.
-- We may improve this situation in GHC at some point, but for now this is just a PoC.
-- The 'targetId' is specified as a 'TargetFile' because 'TargetModule' causes GHC to look for the file anyway, but we
-- use a dummy to avoid having to write the contents to speed up tests.
pureTarget :: FilePath -> UnitId -> ModuleSpec -> Target
pureTarget dummyFile targetUnitId ModuleSpec {..} =
  Target {
    targetId = TargetFile dummyFile Nothing,
    targetAllowObjCode = False,
    targetUnitId,
    targetContents = Just (stringBufferFromByteString content, UTCTime (YearMonthDay 2000 1 1) 0)
  }

-- | Write a source file for the given module and create a GHC target.
fileTarget :: FilePath -> UnitId -> ModuleSpec -> IO Target
fileTarget src targetUnitId ModuleSpec {..} = do
  let dir = src </> unitIdString targetUnitId
  createDirectoryIfMissing True dir
  let path = dir </> name <.> (if boot then "hs-boot" else "hs")
  ByteString.writeFile path content
  pure Target {
    targetId = TargetFile path Nothing,
    targetAllowObjCode = False,
    targetUnitId,
    targetContents = Nothing
  }

-- | Create in-memory GHC targets for all modules in the given unit.
pureUnitTargets :: FilePath -> UnitSpec -> NonEmpty Target
pureUnitTargets dummyFile UnitSpec {name, modules} =
  pureTarget dummyFile (stringToUnitId name) <$> modules

-- | Create file-backed GHC targets for all modules in the given unit.
fileUnitTargets :: FilePath -> UnitSpec -> IO (NonEmpty Target)
fileUnitTargets src UnitSpec {name, modules} = do
  traverse (fileTarget src (stringToUnitId name)) modules
