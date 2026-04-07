module GhcServer.Build.Compile where

import qualified Data.Map.Strict as Map
import GHC (ModuleName, mkModule, moduleNameString)
import GHC.Unit.Types (stringToUnit)
import GhcServer.Cache (loadHomeUnitCache)
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.Unit (Project (..), Unit (..), UnitName (..))
import GhcServer.Log (withBuildLog)
import Internal.Compile.Make (compileModuleWithDepsInHpt)
import Internal.Session (withGhcMakeModule)
import Prelude hiding (log)
import System.Directory.OsPath (createDirectoryIfMissing)
import System.OsPath ((</>))
import System.OsPath.Extra (toOsPath)
import Types.Args (Args (..))
import Types.BuckArgs (IsInterpreted (..))
import Types.CachedDeps (CachedDeps)
import Types.Env (Env (..))
import Types.Log (Logger (..))
import Types.Target (ModuleTarget (..), TargetSpec (..))

-- | Construct a 'ModuleTarget' for a named module in a given unit.
moduleTarget :: UnitName -> ModuleName -> ModuleTarget
moduleTarget name modName =
  ModuleTarget {
    mod = mkModule (stringToUnit name.string) modName
  }

-- | Compile a single module within a unit.
--
-- The caller provides the pre-assembled 'CachedDeps' (computed from the module map)
-- which are passed to the worker for HPT pre-population.
compileSingleModule ::
  BuildEnv ->
  UnitName ->
  ModuleName ->
  CachedDeps ->
  IO ([(UnitName, ModuleName, String)], [String])
compileSingleModule buildEnv name modName cachedDeps =
  case Map.lookup name buildEnv.project.units of
    Nothing -> pure ([(name, modName, "Unit not found in project")], [])
    Just unit -> do
      let modTmpDir = buildEnv.tmpDir </> toOsPath name.string </> toOsPath (moduleNameString modName)
      createDirectoryIfMissing True modTmpDir
      cachedUnit <- loadHomeUnitCache unit.cache
      withBuildLog \ logger -> do
        let
          args = buildEnv.baseArgs {
            tempDir = Just modTmpDir,
            homeUnit = cachedUnit,
            cachedDeps = Just cachedDeps
          }
          env = Env {log = logger, state = buildEnv.stateVar, args}
          target = moduleTarget name modName
        result <- withGhcMakeModule Compiled target env \ _targetSpec ->
          compileModuleWithDepsInHpt logger (TargetModule target)
        captured <- logger.flush
        case result of
          Just _ -> pure ([], captured)
          Nothing -> pure ([(name, modName, "Compilation failed:\n" ++ unlines captured)], [])
