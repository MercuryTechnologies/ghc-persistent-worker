module GhcServer.Build.Metadata where

import Control.Monad.Extra (ifM)
import qualified Data.Map.Strict as Map
import GhcServer.Cache (buildDepPlans, writeUnitCache)
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.BuildEvent (BuildEvent (..), logEvent)
import GhcServer.Data.Unit (Project (..), Unit (..), UnitName (..))
import GhcServer.Path (fp, osPath)
import Internal.Metadata (computeMetadata)
import Prelude hiding (log)
import System.Environment (lookupEnv)
import System.OsPath (OsPath, (</>))
import System.OsPath.Extra (toOsPath)
import Types.Args (Args (..))
import Types.BuildPlan.Incremental (BuckHashesPath (..), BuildPlanPath (..), IncrementalStatePath (..))
import Types.CachedDeps (CachedBuildPlans)
import Types.Env (Env (..))
import Types.Log (Logger (..))

-- | Static GHC arguments used for every metadata step, matching the flags used by the Buck rules and property test.
--
-- These enable dynamic linking, bytecode generation, and explicit package management.
staticMetaArgs :: [String]
staticMetaArgs =
  [
    "-i",
    "-hide-all-packages",
    "-include-pkg-deps",
    "-no-link",
    "-dynamic",
    "-fbyte-code-and-object-code",
    "-fprefer-byte-code",
    "-fPIC",
    "-osuf", "dyn_o",
    "-hisuf", "dyn_hi",
    "-package", "base"
  ]

-- | Construct the GHC CLI arguments for a metadata step.
metadataArgs :: Args -> OsPath -> Maybe CachedBuildPlans -> Maybe OsPath -> Unit -> Args
metadataArgs base outputDir cachedPlans buckHashes unit =
  base {
    sourceHashes = BuckHashesPath <$> buckHashes,
    buildPlan = Just (BuildPlanPath buildPlanPath),
    incrementalState = Just (IncrementalStatePath incrementalStatePath),
    cachedBuildPlans = cachedPlans,
    ghcOptions =
      staticMetaArgs
      ++ unit.ghcArgs
      ++ depFlags
      ++ [
        "-this-unit-id", unit.name.string,
        "-odir", fp outDir,
        "-hidir", fp outDir,
        "-stubdir", fp outDir,
        "-dep-makefile", "/dev/null"
      ]
  }
  where
    buildPlanPath = outDir </> osPath "build-plan.json"

    incrementalStatePath = outDir </> osPath "incremental-state.json"

    outDir = outputDir </> osPath unit.name.string

    depFlags = concatMap depFlag unit.depUnits

    depFlag dep = ["-package-id", dep.string]

-- | Run the metadata step for a unit.
--
-- On success, writes the unit's cache files (args + 'CachedUnit' JSON) so that subsequent builds
-- can restore the unit without rerunning metadata.
--
-- Returns errors (empty on success) and the captured build log.
runMetadata :: BuildEnv -> UnitName -> IO ([(UnitName, String)], [String])
runMetadata buildEnv name = do
  buildEnv.log.debug ("Metadata: " ++ name.string)
  logEvent buildEnv.events (MetadataRan name)
  case Map.lookup name buildEnv.project.units of
    Nothing -> pure ([(name, "Unit not found in project")], [])
    Just unit -> run unit buildEnv.log
  where
    run unit logger = do
      cachedPlans <- buildDepPlans buildEnv.project.depGraph unit
      buckHashes <- lookupEnv "buck_source_hashes"
      let args = metadataArgs buildEnv.baseArgs buildEnv.outputDir (Just cachedPlans) (toOsPath <$> buckHashes) unit
          sourcePaths = map fp unit.sources
          env = Env {
            log = logger,
            state = buildEnv.stateVar,
            args = args {ghcOptions = args.ghcOptions ++ sourcePaths}
          }
      ifM (fst <$> computeMetadata env) (success unit (Just cachedPlans) args logger) (failure logger)

    success unit cachedPlans args logger = do
      cacheResult <- case args.buildPlan of
        Nothing -> pure (Right ())
        Just buildPlan -> writeUnitCache logger unit.cache cachedPlans buildPlan args.ghcOptions
      case cacheResult of
        Left err -> do
          captured <- logger.flush
          pure ([(name, "Cache write failed: " ++ err)], captured)
        Right () -> do
          captured <- logger.flush
          pure ([], captured)

    failure logger = do
      details <- unlines <$> logger.flush
      pure ([(name, "Metadata failed:\n" ++ details)], [])
