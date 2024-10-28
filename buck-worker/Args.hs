module Args where

import Control.Applicative ((<|>))
import Data.Foldable (for_)
import Data.Int (Int32)
import Data.List (dropWhileEnd)
import Data.Map (Map)
import Data.Map.Strict ((!?))
import Internal.AbiHash (AbiHash (..))
import qualified Internal.Args
import Internal.Args (Args (Args))

data CompileResult =
  CompileResult {
    abiHash :: Maybe AbiHash
  }
  deriving stock (Eq, Show)

data BuckArgs =
  BuckArgs {
    topdir :: Maybe String,
    abiOut :: Maybe String,
    buck2Dep :: Maybe String,
    buck2PackageDb :: [String],
    buck2PackageDbDep :: Maybe String,
    binPath :: [String],
    tempDir :: Maybe String,
    ghcDirFile :: Maybe String,
    ghcDbFile :: Maybe String,
    ghcOptions :: [String]
  }
  deriving stock (Eq, Show)

emptyBuckArgs :: Map String String -> BuckArgs
emptyBuckArgs env =
  BuckArgs {
    topdir = Nothing,
    abiOut = Nothing,
    buck2Dep = Nothing,
    buck2PackageDb = [],
    buck2PackageDbDep = Nothing,
    binPath = [],
    tempDir = env !? "TMPDIR",
    ghcDirFile = Nothing,
    ghcDbFile = Nothing,
    ghcOptions = []
  }

parseBuckArgs :: Map String String -> [String] -> Either String BuckArgs
parseBuckArgs env =
  spin (emptyBuckArgs env)
  where
    spin z = \case
      "--abi-out" : rest -> takeArg "--abi-out" rest \ v -> z {abiOut = Just v}
      "--buck2-dep" : rest -> takeArg "--buck2-dep" rest \ v -> z {buck2Dep = Just v}
      "--buck2-package-db" : rest -> takeArg "--buck2-package-db" rest \ v -> z {buck2PackageDb = v : z.buck2PackageDb}
      "--buck2-packagedb-dep" : rest -> takeArg "--buck2-packagedb-dep" rest \ v -> z {buck2PackageDbDep = Just v}
      "--ghc" : rest -> takeArg "--ghc" rest \ _ -> z {ghcOptions = []}
      "--ghc-dir" : rest -> takeArg "--ghc-dir" rest \ f -> z {ghcOptions = [], ghcDirFile = Just f}
      "--extra-pkg-db" : rest -> takeArg "--extra-pkg-db" rest \ f -> z {ghcOptions = [], ghcDbFile = Just f}
      "--bin-path" : rest -> takeArg "--bin-path" rest \ v -> z {binPath = v : z.binPath}
      "-c" : rest -> spin z rest
      ('-' : 'B' : path) : rest -> spin z {topdir = Just path} rest
      arg : rest -> spin z {ghcOptions = arg : z.ghcOptions} rest
      [] -> Right z {ghcOptions = reverse z.ghcOptions}

    takeArg name argv store = case argv of
      [] -> Left (name ++ " needs an argument")
      arg : rest -> spin (store arg) rest

toGhcArgs :: BuckArgs -> IO Args
toGhcArgs args = do
  topdir <- readPath args.ghcDirFile <|> pure args.topdir
  packageDb <- readPath args.ghcDbFile
  pure Args {
    topdir,
    binPath = args.binPath,
    tempDir = args.tempDir,
    ghcOptions = args.ghcOptions ++ foldMap packageDbArg packageDb ++ foldMap packageDbArg args.buck2PackageDb
  }
  where
    packageDbArg path = ["-package-db", path]
    readPath = fmap (fmap (dropWhileEnd ('\n' ==))) . traverse readFile

writeResult :: BuckArgs -> Maybe CompileResult -> IO Int32
writeResult args = \case
  Nothing -> pure 1
  Just CompileResult {abiHash} -> do
    for_ abiHash \ AbiHash {path, hash} -> writeFile path hash
    for_ args.buck2Dep \ path -> writeFile path "\n"
    for_ args.buck2PackageDbDep \ path ->
      case args.buck2PackageDb of
        [] -> writeFile path "\n"
        dbs -> writeFile path (unlines dbs)
    pure 0
