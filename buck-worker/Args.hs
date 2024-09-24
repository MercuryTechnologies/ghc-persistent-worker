{-# language BlockArguments, LambdaCase, DerivingStrategies, RecordWildCards, OverloadedRecordDot #-}

module Args where

import Data.Foldable (for_)
import AbiHash (AbiHash (..))
import Data.Int (Int32)

data Args =
  Args {
    abiOut :: Maybe String,
    buck2Dep :: Maybe String,
    buck2PackageDb :: [String],
    buck2PackageDbDep :: Maybe String,
    ghcDirFile :: Maybe String,
    ghcDbFile :: Maybe String,
    ghcOptions :: [String]
  }
  deriving stock (Eq, Show)

data CompileResult =
  CompileResult {
    abiHash :: Maybe AbiHash
  }
  deriving stock (Eq, Show)

emptyArgs :: Args
emptyArgs =
  Args {
    abiOut = Nothing,
    buck2Dep = Nothing,
    buck2PackageDb = [],
    buck2PackageDbDep = Nothing,
    ghcDirFile = Nothing,
    ghcDbFile = Nothing,
    ghcOptions = []
  }

parseBuckArgs :: [String] -> Either String Args
parseBuckArgs =
  spin emptyArgs
  where
    spin Args {..} = \case
      "--abi-out" : rest -> takeArg "--abi-out" rest \ v -> Args {abiOut = Just v, ..}
      "--buck2-dep" : rest -> takeArg "--buck2-dep" rest \ v -> Args {buck2Dep = Just v, ..}
      "--buck2-packagedb" : rest -> takeArg "--buck2-packagedb" rest \ v -> Args {buck2PackageDb = v : buck2PackageDb, ..}
      "--buck2-packagedb-dep" : rest -> takeArg "--buck2-packagedb-dep" rest \ v -> Args {buck2PackageDbDep = Just v, ..}
      "--ghc" : rest -> takeArg "--ghc" rest \ _ -> Args {ghcOptions = [], ..}
      "--ghc-dir" : rest -> takeArg "--ghc-dir" rest \ f -> Args {ghcOptions = [], ghcDirFile = Just f, ..}
      "--extra-pkg-db" : rest -> takeArg "--extra-pkg-db" rest \ f -> Args {ghcOptions = [], ghcDbFile = Just f, ..}
      "-c" : rest -> spin Args {ghcOptions = ghcOptions, ..} rest
      arg : rest -> spin Args {ghcOptions = arg : ghcOptions, ..} rest
      [] -> Right Args {ghcOptions = reverse ghcOptions, ..}

    takeArg name argv store = case argv of
      [] -> Left (name ++ " needs an argument")
      arg : rest -> spin (store arg) rest

writeResult :: Args -> Maybe CompileResult -> IO Int32
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
