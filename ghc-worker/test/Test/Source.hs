module Test.Source where

import Data.ByteString.Lazy (ByteString)
import Data.Foldable (for_)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Encoding (encodeUtf8)
import System.Directory.OsPath (createDirectoryIfMissing)
import qualified System.File.OsPath as OsPath
import System.OsPath (OsPath, (</>))
import Test.Data.Project (ErrorVariant (..), ModuleKey (..))
import Test.Data.SourceMode (SourceMode (..))
import Test.Path (moduleName, moduleSourcePath, moduleValueName, unitDir)

sumExpr :: String -> [String] -> String
sumExpr base = \case
  [] -> base
  names -> intercalate " + " (base : names)

-- | Write a source file for a module according to specifications.
--
-- Each module exports a @value_X_Y :: Int@ binding whose expression sums the corresponding values imported from the
-- dependencies, ensuring imports are actually used and type-checked by GHC.
moduleSource :: SourceMode -> ModuleKey -> [ModuleKey] -> ByteString
moduleSource mode key deps =
  encodeUtf8 $
  Text.pack $
  unlines $
    ["module " ++ headerName ++ " where", ""]
    ++ ["import " ++ moduleName d | d <- deps]
    ++ [valName ++ " :: Int", valName ++ " = " ++ valueExpr]
  where
    headerName = case mode of
      SourceFixed -> moduleName (key {errorVariant = Nothing})
      _ -> moduleName key
    base = case mode of
      SourceNormal -> maybe "1" errorBase key.errorVariant
      SourceModified -> "100"
      SourceFixed -> "1"
    errorBase UndefinedVariable = "x"
    errorBase TypeMismatch = "True"
    valName = moduleValueName key
    valueExpr = sumExpr base (moduleValueName <$> deps)

-- | Write source files for all modules.
writeProjectSources :: OsPath -> Map ModuleKey [ModuleKey] -> IO ()
writeProjectSources srcDir modules =
  for_ (Map.toList modules) \ (key, deps) -> do
    createDirectoryIfMissing True (srcDir </> unitDir key.unit)
    OsPath.writeFile (srcDir </> moduleSourcePath key) (moduleSource SourceNormal key deps)
