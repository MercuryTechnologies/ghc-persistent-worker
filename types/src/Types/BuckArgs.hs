module Types.BuckArgs where

import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Data.Aeson (FromJSON, eitherDecodeFileStrict')
import Data.Coerce (coerce)
import Data.List (dropWhileEnd)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import Data.Maybe (fromMaybe, isJust)
import GHC (mkModule, mkModuleName)
import GHC.Unit (Definite (..), GenUnit (RealUnit), stringToUnitId)
import System.FilePath (takeDirectory)
import qualified Types.Args
import Types.Args (Args (Args), TargetId (..), UnitName (..))
import Types.Grpc (CommandEnv (..), RequestArgs (..))
import Types.Target (ModuleTarget (..))

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
    unit :: Maybe String,
    moduleName :: Maybe String,
    depModules :: Maybe String,
    depUnits :: Maybe String,
    homeUnit :: Maybe String,
    workerTargetId :: Maybe TargetId,
    pluginDb :: Maybe String,
    env :: Map String String,
    binPath :: [String],
    tempDir :: Maybe String,
    ghcDirFile :: Maybe String,
    ghcDbFile :: Maybe String,
    ghcArgsFile :: Maybe String,
    ghcOptions :: [String],
    multiplexerCustom :: Bool,
    mode :: Maybe Mode,
    envKey :: Maybe String,
    closeInput :: Maybe String,
    closeOutput :: Maybe String
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
    unit = Nothing,
    moduleName = Nothing,
    depModules = Nothing,
    depUnits = Nothing,
    homeUnit = Nothing,
    workerTargetId = Nothing,
    pluginDb = Nothing,
    env,
    binPath = [],
    tempDir = env !? "TMPDIR",
    ghcDirFile = Nothing,
    ghcDbFile = Nothing,
    ghcArgsFile = Nothing,
    ghcOptions = [],
    multiplexerCustom = False,
    mode = Nothing,
    envKey = Nothing,
    closeInput = Nothing,
    closeOutput = Nothing
  }

options :: Map String ([String] -> BuckArgs -> Either String ([String], BuckArgs))
options =
  [
    withArg "--abi-out" \ z a -> z {abiOut = Just a},
    withArg "--buck2-dep" \ z a -> z {buck2Dep = Just a},
    withArg "--buck2-package-db" \ z a -> z {buck2PackageDb = a : z.buck2PackageDb},
    withArg "--buck2-packagedb-dep" \ z a -> z {buck2PackageDbDep = Just a},
    withArg "--dep-modules" \ z a -> z {depModules = Just a},
    withArg "--dep-units" \ z a -> z {depUnits = Just a},
    withArg "--home-unit" \ z a -> z {homeUnit = Just a},
    withArg "--extra-env-key" \ z a -> z {envKey = Just a},
    withArgErr "--extra-env-value" \ z a -> addEnv z a,
    withArg "--worker-target-id" \ z a -> z {workerTargetId = Just (TargetId a)},
    withArg "--worker-socket" const,
    withArg "--plugin-db" \ z a -> z {pluginDb = Just a},
    withArg "--ghc-dir" \ z a -> z {ghcDirFile = Just a},
    withArg "--unit" \ z a -> z {unit = Just a},
    withArg "--module" \ z a -> z {moduleName = Just a, mode = Just ModeCompile},
    withArg "--ghc-args" \ z a -> z {ghcArgsFile = Just a},
    withArg "--extra-pkg-db" \ z a -> z {ghcDbFile = Just a},
    withArg "--bin-path" \ z a -> z {binPath = a : z.binPath},
    withArg "--bin-exe" \ z a -> z {binPath = takeDirectory a : z.binPath},
    withArg "--worker-mode" \ z a -> z {mode = Just (parseMode a)},
    flag "--worker-multiplexer-custom" \ z -> z {multiplexerCustom = True},
    withArg "--close-input" \z a -> z {closeInput = Just a},
    withArg "--close-output" \z a -> z {closeOutput = Just a},
    ("-c", \ rest z -> Right (rest, z {mode = Just ModeCompile})),
    ("-M", \ rest z -> Right (rest, z {mode = Just ModeMetadata}))
  ]
  where
    addEnv z a = case z.envKey of
      Just key -> Right z {env = Map.insert key a z.env, envKey = Nothing}
      Nothing -> Left ("--extra-env-value used without preceding --extra-env-key (arg: " ++ a ++ ")")

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

decodeJsonArg ::
  FromJSON a =>
  String ->
  String ->
  IO a
decodeJsonArg desc file =
  eitherDecodeFileStrict' file >>= \case
    Right a -> pure a
    Left err -> throwIO (userError ("Invalid JSON in file for " ++ desc ++ ": " ++ err ++ " (" ++ file ++ ")"))

-- | @CompileHpt@ can either process a source file or pick a previously constructed @ModSummary@ from the module graph.
-- In the latter case, we need both a unit ID and a module name, which is ensured here.
checkModuleTarget ::
  BuckArgs ->
  IO (Maybe ModuleTarget)
checkModuleTarget args =
  case (args.unit, args.moduleName) of
    (Nothing, Just _) ->
      throwIO (userError "Specified --module without --unit")
    (Just unit, Just name) ->
      pure (Just (ModuleTarget (mkModule (RealUnit (Definite (stringToUnitId unit))) (mkModuleName name))))
    _ ->
      pure Nothing

toGhcArgs :: BuckArgs -> IO Args
toGhcArgs args = do
  cachedDeps <- traverse (decodeJsonArg "--dep-modules") args.depModules
  cachedBuildPlans <- traverse (decodeJsonArg "--dep-units") args.depUnits
  homeUnit <- traverse (decodeJsonArg "--home-unit") args.homeUnit
  topdir <- (<|> args.topdir) <$> readPath args.ghcDirFile
  packageDb <- readPath args.ghcDbFile
  -- When a module name was specified, we don't read any args because we can't use them when picking @ModSummary@ from
  -- the module graph.
  ghcArgs <-
    if isJust args.moduleName
    then pure [] else
    maybe args.ghcOptions lines <$> traverse readFile args.ghcArgsFile
  moduleTarget <- checkModuleTarget args
  pure Args {
    topdir,
    workerTargetId = args.workerTargetId,
    binPath = args.binPath,
    tempDir = args.tempDir,
    unit = UnitName . stringToUnitId <$> args.unit,
    moduleTarget,
    ghcOptions = ghcArgs ++ foldMap packageDbArg packageDb ++ foldMap packageDbArg args.buck2PackageDb,
    cachedBuildPlans,
    cachedDeps,
    homeUnit
  }
  where
    packageDbArg path = ["-package-db", path]
    readPath = fmap (fmap (dropWhileEnd ('\n' ==))) . traverse readFile

-- | Arguments interpreted by the worker directly that need to be applied again when restoring module graphs from cache.
data CachedBuckArgs =
  CachedBuckArgs {
    cachedBinPath :: [String]
  }
  deriving stock (Eq, Show)

emptyCachedBuckArgs :: CachedBuckArgs
emptyCachedBuckArgs =
  CachedBuckArgs {
    cachedBinPath = []
  }

cachedOptions :: Map String ([String] -> CachedBuckArgs -> Either String ([String], CachedBuckArgs))
cachedOptions =
  [
    withArg "--bin-path" \ z a -> z {cachedBinPath = a : z.cachedBinPath},
    withArg "--bin-exe" \ z a -> z {cachedBinPath = takeDirectory a : z.cachedBinPath}
  ]
  where
    withArg name f = (name, \ argv z -> takeArg name argv (Right . f z))

    takeArg name argv store = case argv of
      [] -> Left (name ++ " needs an argument")
      arg : rest -> do
        new <- store arg
        Right (rest, new)

parseCachedBuckArgs :: [String] -> Either String CachedBuckArgs
parseCachedBuckArgs =
  spin emptyCachedBuckArgs
  where
    spin z = \case
      arg : args -> do
        (rest, new) <- fromMaybe (equalsArg arg) (cachedOptions !? arg) args z
        spin new rest
      [] -> Right z

    equalsArg arg rest z
      | (name, '=' : value) <- break ('=' ==) arg
      , Just handler <- cachedOptions !? name
      = handler (value : rest) z
      | otherwise
      = Left ("Unknown option in cached Buck args: " ++ arg)
