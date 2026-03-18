module Test.Path where

import Control.Monad.Extra (whenM)
import System.Directory.OsPath (doesFileExist, removeFile)
import System.OsPath (OsPath, decodeUtf, osp, unsafeEncodeUtf, (<.>), (</>))
import System.OsPath (OsPath, osp, unsafeEncodeUtf, (<.>), (</>))
import Test.Data.Project (ModuleKey (..), UnitKey (..))

-- * Path Converters

fp :: OsPath -> FilePath
fp p =
  either (error . msg) id (decodeUtf p)
  where
    msg err = "Decoding path " <> show p <> " failed: " <> show err

osPath :: String -> OsPath
osPath = unsafeEncodeUtf

-- * Unit Names and Paths

showUnit :: UnitKey -> String
showUnit (UnitKey key) = show key

unitName :: UnitKey -> String
unitName unit = "unit" ++ showUnit unit

unitDir :: UnitKey -> OsPath
unitDir = osPath . unitName

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

moduleOutputBase :: ModuleKey -> OsPath
moduleOutputBase key =
  unitOutputDir key.unit </> osPath (moduleName key)

moduleSourcePath :: ModuleKey -> OsPath
moduleSourcePath key =
  unitDir key.unit </> osPath (moduleName key) <.> [osp|hs|]

compileTmpDir :: ModuleKey -> OsPath
compileTmpDir key =
  unitDir key.unit </> osPath (moduleName key)

cachedUnitPath :: UnitKey -> OsPath
cachedUnitPath unit =
  unitCacheDir unit </> [osp|cached_unit.json|]

removeIfExists :: OsPath -> IO ()
removeIfExists path = do
  whenM (doesFileExist path) do
    removeFile path
