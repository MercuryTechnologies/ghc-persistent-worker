module Test.Source where

import Data.ByteString.Lazy (ByteString)
import Data.Foldable (for_, toList)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Encoding (encodeUtf8)
import System.Directory.OsPath (createDirectoryIfMissing)
import qualified System.File.OsPath as OsPath
import System.OsPath.Extra (OsPath, (</>))
import Test.Data.Project (ErrorVariant (..), ModuleKey (..), ModuleSource (..))
import Test.Data.SourceMode (SourceMode (..))
import Test.Path (
  extDepModuleName,
  extDepValueName,
  indexedValueName,
  moduleName,
  moduleSourcePath,
  moduleValueName,
  unitDir,
  )

sumExpr :: String -> [String] -> String
sumExpr base = \case
  [] -> base
  names -> intercalate " + " (base : names)

-- | The expression for one value binding in the generated module.
--
-- When TH is enabled, values imported from dependencies are wrapped in a splice to trigger bytecode linking.
valueExpr :: Bool -> String -> [String] -> String
valueExpr useTh base depValues =
  sumExpr base (wrapValue <$> depValues)
  where
    wrapValue v
      | useTh = "$(lift @_ @Int " ++ v ++ ")"
      | otherwise = v

-- | Write a source file for a module according to specifications.
--
-- Each module exports @bindings@ value bindings whose expressions sum the corresponding values imported from the
-- dependencies, ensuring imports are actually used and type-checked by GHC.
-- The primary binding is @value_X_Y@; additional bindings are @value_X_Y_1@, @value_X_Y_2@, etc.
moduleSource :: ModuleSource -> SourceMode -> ModuleKey -> ByteString
moduleSource ModuleSource {..} mode key =
  encodeUtf8 $
  Text.pack $
  unlines $
    thPragma
    ++ ["module " ++ headerName ++ " where", ""]
    ++ thImport
    ++ ["import " ++ moduleName d | d <- deps]
    ++ ["import " ++ extDepModuleName i | i <- toList extDeps]
    ++ concatMap valueBinding (enumFromTo 0 (bindings - 1))
  where
    (thPragma, thImport)
      | th = (["{-# LANGUAGE TemplateHaskell #-}"], ["import Language.Haskell.TH.Syntax (lift)"])
      | otherwise = ([], [])

    headerName = case mode of
      SourceFixed -> moduleName (key {errorVariant = Nothing})
      _ -> moduleName key

    base = case mode of
      SourceNormal -> maybe "1" errorBase key.errorVariant
      SourceModified -> "100"
      SourceFixed -> "1"

    errorBase = \case
      UndefinedVariable -> "x"
      TypeMismatch -> "True"

    allDepValues = (moduleValueName <$> deps) ++ (extDepValueName <$> toList extDeps)

    valueBinding i =
      [valName i ++ " :: Int", valName i ++ " = " ++ valueExpr th base allDepValues]

    valName i
      | i == 0 = moduleValueName key
      | otherwise = indexedValueName key i

-- | Write source files for all modules.
writeProjectSources :: OsPath -> Map ModuleKey ModuleSource -> IO ()
writeProjectSources srcDir modules =
  for_ (Map.toList modules) \ (key, ms) -> do
    createDirectoryIfMissing True (srcDir </> unitDir key.unit)
    OsPath.writeFile (srcDir </> moduleSourcePath key) (moduleSource ms SourceNormal key)
