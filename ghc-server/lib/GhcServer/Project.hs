module GhcServer.Project where

import Control.Monad (when)
import Data.Aeson (eitherDecodeStrict')
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.ByteString as BS
import GHC.Data.Graph.Directed (graphFromEdgedVerticesOrd)
import qualified GHC.Data.Graph.Directed as Graph (Node (..))
import GhcServer.Data.Unit (Project (..), Unit (..), UnitCache (..), UnitDepNode, UnitName (..), mkUnitCache)
import GhcServer.Data.UnitConfig (UnitConfig (..))
import GhcServer.Path (osPath)
import System.Directory.OsPath (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.OsPath (OsPath, decodeUtf, encodeUtf, (</>))

-- | Build a 'UnitDepNode' for the dependency graph from a 'Unit'.
--
-- The node payload is the unit's own @cached_unit.json@ path; edges point to its direct dep unit names.
unitDepNode :: Unit -> UnitDepNode
unitDepNode unit =
  Graph.DigraphNode {
    node_payload = unit.cache.cachedUnitPath,
    node_key = unit.name,
    node_dependencies = unit.depUnits
  }

-- | The name of the JSON config file marking a unit directory.
unitConfigPath :: OsPath
unitConfigPath =
  case encodeUtf "unit.json" of
    Right p -> p
    Left e -> error ("Failed to encode 'unit.json': " ++ show e)

-- | Read and parse a @unit.json@ file.
readUnitConfig :: OsPath -> IO UnitConfig
readUnitConfig path = do
  fp <- either (fail . show) pure (decodeUtf path)
  bytes <- BS.readFile fp
  either fail pure (eitherDecodeStrict' bytes)

-- | Check whether an 'OsPath' has a @.hs@ extension.
isHaskellSource :: OsPath -> Bool
isHaskellSource path =
  case decodeUtf path of
    Right s -> ".hs" `isSuffixOf` s
    Left _ -> False

-- | Discover a single unit from a directory that contains a @unit.json@ file.
--
-- Creates the unit's output and temp directories if the unit is found.
discoverUnit :: OsPath -> OsPath -> OsPath -> OsPath -> IO (Maybe Unit)
discoverUnit projectRoot outputDir tmpDir name = do
  let dir = projectRoot </> name
  let configFile = dir </> unitConfigPath
  doesFileExist configFile >>= \case
    False -> pure Nothing
    True -> do
      config <- readUnitConfig configFile
      entries <- listDirectory dir
      let sources = [dir </> e | e <- entries, isHaskellSource e]
      nameStr <- either (fail . show) pure (decodeUtf name)
      let unitName = UnitName nameStr
      createDirectoryIfMissing True (outputDir </> osPath nameStr)
      createDirectoryIfMissing True (tmpDir </> osPath nameStr)
      pure (Just Unit {
        name = unitName,
        dir,
        ghcArgs = config.args,
        sources,
        depUnits = UnitName <$> config.deps,
        cache = mkUnitCache projectRoot unitName
      })

-- | Discover all units in the project root, creating output and temp directories.
--
-- Throws if no units are found.
discoverProject :: OsPath -> OsPath -> OsPath -> IO Project
discoverProject projectRoot outputDir tmpDir = do
  createDirectoryIfMissing True outputDir
  createDirectoryIfMissing True tmpDir
  entries <- listDirectory projectRoot
  units <- catMaybes <$> traverse (discoverUnit projectRoot outputDir tmpDir) entries
  when (null units) do
    let root = either (const "<decode error>") id (decodeUtf projectRoot)
    fail ("No units found in project root: " ++ root)
  let unitMap = Map.fromList [(u.name, u) | u <- units]
      depGraph = graphFromEdgedVerticesOrd (map unitDepNode units)
  pure Project {units = unitMap, depGraph}
