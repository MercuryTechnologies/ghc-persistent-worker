{-# LANGUAGE QuasiQuotes #-}

module Test.BuckHashes where

import qualified Data.Aeson as Aeson
import Data.Aeson (encodeFile)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.ByteString.Lazy (LazyByteString)
import Data.Hashable (hash)
import Data.String (fromString)
import Data.Traversable (for)
import System.Directory.OsPath (createDirectoryIfMissing)
import qualified System.File.OsPath as OsPath
import System.OsPath (OsPath, osp, (</>))
import System.OsPath.Extra (fromOsPath)
import Test.Data.Project (BuildModule (..), GenUnit (..))
import Test.Path (moduleSourcePath, unitDir)
import Types.BuildPlan.Incremental (BuckHash (..), BuckHashes (..), BuckHashesPath (..), SourceHash)

-- | Compute a deterministic hash string from file content.
-- Uses a simple digest based on content bytes, matching the format used by Buck (@hash:size@).
fileHash :: LazyByteString -> SourceHash
fileHash content =
  fromString (show contentHash ++ ":" ++ show len)
  where
    len = LazyByteString.length content
    contentHash = hash content

computeHashes :: OsPath -> [GenUnit BuildModule] -> IO [BuckHash]
computeHashes sourceDir units =
  concat <$> for units \ unit ->
    for unit.modules \ m -> do
      let path = sourceDir </> moduleSourcePath m.key
      content <- OsPath.readFile path
      pure BuckHash {path, digest = fileHash content}

writeHashes :: OsPath -> OsPath -> [GenUnit BuildModule] -> IO FilePath
writeHashes metadataDir sourceDir units = do
  digests <- computeHashes sourceDir units
  let meta = BuckHashes {version = 1, digests}
      metaPath = fromOsPath (metadataDir </> [osp|source_hashes.json|])
  encodeFile metaPath meta
  pure metaPath

writeHashesFromPaths :: BuckHashesPath -> [OsPath] -> IO LazyByteString
writeHashesFromPaths (BuckHashesPath hashesPath) paths = do
  digests <- for paths \ path -> do
    content <- OsPath.readFile path
    pure BuckHash {path, digest = fileHash content}
  let content = Aeson.encode BuckHashes {version = 1, digests}
  OsPath.writeFile hashesPath content
  pure content

-- | Write source hashes for a unit at @tempDir/unitN/source_hashes.json@.
writeUnitHashes :: OsPath -> OsPath -> GenUnit BuildModule -> IO LazyByteString
writeUnitHashes tempDir sourceDir unit = do
  createDirectoryIfMissing True metaDir
  writeHashesFromPaths hashesPath paths
  where
    metaDir = tempDir </> unitDir unit.key
    paths = [sourceDir </> moduleSourcePath m.key | m <- unit.modules]
    hashesPath = BuckHashesPath (metaDir </> [osp|source_hashes.json|])
