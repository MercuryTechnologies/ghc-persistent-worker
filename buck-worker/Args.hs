{-# language OverloadedLists #-}

module Args where

import Control.Applicative ((<|>))
import Data.Foldable (for_)
import Data.Int (Int32)
import Data.List (dropWhileEnd)
import Data.Map (Map)
import Data.Map.Strict ((!?))
import Data.Maybe (fromMaybe)
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

options :: Map String ([String] -> BuckArgs -> Either String ([String], BuckArgs))
options =
  [
    withArg "--abi-out" \ z a -> z {abiOut = Just a},
    withArg "--buck2-dep" \ z a -> z {buck2Dep = Just a},
    withArg "--buck2-package-db" \ z a -> z {buck2PackageDb = a : z.buck2PackageDb},
    withArg "--buck2-packagedb-dep" \ z a -> z {buck2PackageDbDep = Just a},
    withArg "--ghc" \ z _ -> z {ghcOptions = []},
    withArg "--ghc-dir" \ z a -> z {ghcDirFile = Just a},
    withArg "--extra-pkg-db" \ z a -> z {ghcDbFile = Just a},
    withArg "--bin-path" \ z a -> z {binPath = a : z.binPath},
    skip "-c"
  ]
  where
    skip name = (name, \ rest z -> Right (rest, z))

    withArg name f = (name, \ argv z -> takeArg name argv (f z))

    takeArg name argv store = case argv of
      [] -> Left (name ++ " needs an argument")
      arg : rest -> Right (rest, store arg)

parseBuckArgs :: Map String String -> [String] -> Either String BuckArgs
parseBuckArgs env =
  spin (emptyBuckArgs env)
  where
    spin z = \case
      ('-' : 'B' : path) : rest -> spin z {topdir = Just path} rest
      arg : args -> do
        (rest, new) <- fromMaybe (ghcOption arg) (options !? arg) args z
        spin new rest
      [] -> Right z {ghcOptions = reverse z.ghcOptions}

    ghcOption arg rest z = Right (rest, z {ghcOptions = arg : z.ghcOptions})

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
