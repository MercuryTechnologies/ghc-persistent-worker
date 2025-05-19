module BuckArgs where

import Control.Applicative ((<|>))
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.Int (Int32)
import Data.List (dropWhileEnd)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import Data.Maybe (fromMaybe)
import Grpc (CommandEnv (..), RequestArgs (..))
import Internal.AbiHash (AbiHash (..))
import qualified Internal.Args
import Internal.Args (Args (Args))
import Internal.Cache (ModuleArtifacts)
import System.FilePath (takeDirectory)

-- | Right now the 'Maybe' just corresponds to the presence of the CLI argument @--abi-out@ – errors occuring while
-- reading the iface are thrown.
data CompileResult =
  CompileResult {
    artifacts :: ModuleArtifacts,
    abiHash :: Maybe AbiHash
  }
  deriving stock (Show)

data Mode =
  ModeCompile
  |
  ModeLink
  |
  ModeMetadata
  |
  ModeUnknown String
  deriving stock (Eq, Show)

parseMode :: String -> Mode
parseMode = \case
  "compile" -> ModeCompile
  "link" -> ModeLink
  "metadata" -> ModeMetadata
  mode -> ModeUnknown mode

data BuckArgs =
  BuckArgs {
    topdir :: Maybe String,
    abiOut :: Maybe String,
    buck2Dep :: Maybe String,
    buck2PackageDb :: [String],
    buck2PackageDbDep :: Maybe String,
    workerTargetId :: Maybe String,
    pluginDb :: Maybe String,
    env :: Map String String,
    binPath :: [String],
    tempDir :: Maybe String,
    ghcPath :: Maybe String,
    ghcDirFile :: Maybe String,
    ghcDbFile :: Maybe String,
    ghcOptions :: [String],
    multiplexerCustom :: Bool,
    mode :: Maybe Mode,
    envKey :: Maybe String
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
    workerTargetId = Nothing,
    pluginDb = Nothing,
    env,
    binPath = [],
    tempDir = env !? "TMPDIR",
    ghcPath = Nothing,
    ghcDirFile = Nothing,
    ghcDbFile = Nothing,
    ghcOptions = [],
    multiplexerCustom = False,
    mode = Nothing,
    envKey = Nothing
  }

options :: Map String ([String] -> BuckArgs -> Either String ([String], BuckArgs))
options =
  [
    withArg "--abi-out" \ z a -> z {abiOut = Just a},
    withArg "--buck2-dep" \ z a -> z {buck2Dep = Just a},
    withArg "--buck2-package-db" \ z a -> z {buck2PackageDb = a : z.buck2PackageDb},
    withArg "--buck2-packagedb-dep" \ z a -> z {buck2PackageDbDep = Just a},
    withArg "--extra-env-key" \ z a -> z {envKey = Just a},
    withArgErr "--extra-env-value" \ z a -> addEnv z a,
    withArg "--worker-target-id" \ z a -> z {workerTargetId = Just a},
    withArg "--worker-socket" const,
    skip "--worker-close",
    withArg "--plugin-db" \ z a -> z {pluginDb = Just a},
    withArg "--ghc" \ z a -> z {ghcOptions = [], ghcPath = Just a},
    withArg "--ghc-dir" \ z a -> z {ghcDirFile = Just a},
    withArg "--extra-pkg-db" \ z a -> z {ghcDbFile = Just a},
    withArg "--bin-path" \ z a -> z {binPath = a : z.binPath},
    withArg "--bin-exe" \ z a -> z {binPath = takeDirectory a : z.binPath},
    withArg "--worker-mode" \ z a -> z {mode = Just (parseMode a)},
    flag "--worker-multiplexer-custom" \ z -> z {multiplexerCustom = True},
    ("-c", \ rest z -> Right (rest, z {mode = Just ModeCompile})),
    ("-M", \ rest z -> Right (rest, z {mode = Just ModeMetadata}))
  ]
  where
    addEnv z a = case z.envKey of
      Just key -> Right z {env = Map.insert key a z.env, envKey = Nothing}
      Nothing -> Left ("--extra-env-value used without preceding --extra-env-key (arg: " ++ a ++ ")")

    skip name = (name, \ rest z -> Right (rest, z))

    flag name f = (name, \ rest z -> Right (rest, f z))

    withArg name f = (name, \ argv z -> takeArg name argv (Right . f z))

    withArgErr name f = (name, \ argv z -> takeArg name argv (f z))

    takeArg name argv store = case argv of
      [] -> Left (name ++ " needs an argument")
      arg : rest -> do
        new <- store arg
        Right (rest, new)

parseBuckArgs :: CommandEnv -> RequestArgs -> Either String BuckArgs
parseBuckArgs env =
  spin (emptyBuckArgs (coerce env)) . coerce
  where
    spin z = \case
      ('-' : 'B' : path) : rest -> spin z {topdir = Just path} rest
      arg : args -> do
        (rest, new) <- fromMaybe (equalsArg arg) (options !? arg) args z
        spin new rest
      [] -> Right z {ghcOptions = reverse z.ghcOptions}

    -- For @--worker-target-id=worker1@ style args
    equalsArg arg rest z
      | (name, '=' : value) <- break ('=' ==) arg
      , Just handler <- options !? name
      = handler (value : rest) z
      | otherwise
      = ghcOption arg rest z

    -- Let GHC handle the arg
    ghcOption arg rest z = Right (rest, z {ghcOptions = arg : z.ghcOptions})

toGhcArgs :: BuckArgs -> IO Args
toGhcArgs args = do
  topdir <- (<|> args.topdir) <$> readPath args.ghcDirFile
  packageDb <- readPath args.ghcDbFile
  pure Args {
    topdir,
    workerTargetId = args.workerTargetId,
    binPath = args.binPath,
    tempDir = args.tempDir,
    ghcPath = args.ghcPath,
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
