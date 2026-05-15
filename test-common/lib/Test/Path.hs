module Test.Path where

import Control.Monad.Extra (whenM)
import System.Directory.OsPath (doesFileExist, removeFile)
import System.OsPath (OsPath, osp, (<.>), (</>))
import System.OsPath.Extra (toOsPath)
import Test.Data.Project (ModuleKey (..), UnitKey (..))

-- * Unit Names and Paths

showUnit :: UnitKey -> String
showUnit (UnitKey key) = show key

unitName :: UnitKey -> String
unitName unit = "unit" ++ showUnit unit

unitDir :: UnitKey -> OsPath
unitDir = toOsPath . unitName

unitOutputDir :: UnitKey -> OsPath
unitOutputDir key = [osp|out|] </> unitDir key

unitTmpDir :: UnitKey -> OsPath
unitTmpDir key = [osp|meta|] </> unitDir key

unitCacheDir :: UnitKey -> OsPath
unitCacheDir unit = [osp|cache|] </> unitDir unit

-- * Module Names and Paths

moduleName :: ModuleKey -> String
moduleName ModuleKey {unit, number} =
  "Unit" ++ showUnit unit ++ "Module" ++ show number

moduleValueName :: ModuleKey -> String
moduleValueName ModuleKey {unit, number} =
  "value_" ++ showUnit unit ++ "_" ++ show number

-- | Name for the nth additional value binding in a module, starting at index 1.
-- The primary binding (index 0) uses 'moduleValueName'.
indexedValueName :: ModuleKey -> Int -> String
indexedValueName key i =
  moduleValueName key ++ "_" ++ show i

moduleOutputBase :: ModuleKey -> OsPath
moduleOutputBase key =
  unitOutputDir key.unit </> toOsPath (moduleName key)

moduleSourcePath :: ModuleKey -> OsPath
moduleSourcePath key =
  unitDir key.unit </> toOsPath (moduleName key) <.> [osp|hs|]

compileTmpDir :: ModuleKey -> OsPath
compileTmpDir key =
  unitDir key.unit </> toOsPath (moduleName key)

cachedUnitPath :: UnitKey -> OsPath
cachedUnitPath unit =
  unitCacheDir unit </> [osp|cached_unit.json|]

removeIfExists :: OsPath -> IO ()
removeIfExists path = do
  whenM (doesFileExist path) do
    removeFile path

-- * External Dependency Names

-- | Package name for an external dependency, e.g. @"extdep0"@.
extDepName :: Int -> String
extDepName i = "extdep" ++ show i

-- | Module name exported by an external dependency package, e.g. @"Extdep0"@.
extDepModuleName :: Int -> String
extDepModuleName i = "Extdep" ++ show i

-- | Value name exported by an external dependency module, e.g. @"extdep_value_0"@.
extDepValueName :: Int -> String
extDepValueName i = "extdep_value_" ++ show i
