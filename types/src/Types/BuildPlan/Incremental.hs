{-# LANGUAGE DeriveAnyClass, QuasiQuotes, NoFieldSelectors #-}

module Types.BuildPlan.Incremental (
  BuildPlanPath (..),
  BuckHashesPath (..),
  IncrementalStatePath (..),
  SourceHash,
  BuckHash (..),
  BuckHashes (..),
  SourceHashes,
  emptySourceHashes,
  unsafeSourceHashes,
  sourceHashesForTargets,
  storeHashes,
  IncrementalState (..),
  StoredHash,
  SourceChanges (..),
  sourceChanges,
) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Coerce (coerce)
import qualified Data.Map.Merge.Strict as Map
import Data.Map.Merge.Strict (dropMissing, mapMissing, zipWithMatched, zipWithMaybeMatched)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.OsPath.Extra (OsPath)
import Types.BuildPlan (BuildPlanJson)

newtype BuildPlanPath =
  BuildPlanPath { path :: OsPath }
  deriving stock (Eq, Show, Ord)

newtype BuckHashesPath =
  BuckHashesPath { path :: OsPath }
  deriving stock (Eq, Show, Ord)

newtype IncrementalStatePath =
  IncrementalStatePath { path :: OsPath }
  deriving stock (Eq, Show, Ord)

-- | A hash representing the _current_ content of a source file.
newtype SourceHash =
  SourceHash { hash :: Text }
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToJSON, IsString)

-- | Hash representing the _current_ contents of source files.
newtype SourceHashes =
  SourceHashes (Map OsPath SourceHash)
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToJSON)

emptySourceHashes :: SourceHashes
emptySourceHashes = SourceHashes []

unsafeSourceHashes :: SourceHashes -> Map OsPath SourceHash
unsafeSourceHashes = coerce

-- | A single input entry in Buck's source hashes file.
data BuckHash =
  BuckHash {
    path :: OsPath,
    digest :: SourceHash
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Buck's source hashes JSON structure.
--
-- Created by Buck before action execution, listing all inputs with their content hashes.
data BuckHashes =
  BuckHashes {
    version :: Int,
    digests :: [BuckHash]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

sourceHashesForTargets :: BuckHashes -> Set OsPath -> SourceHashes
sourceHashesForTargets BuckHashes {digests} =
  SourceHashes . Map.restrictKeys (Map.fromList [(d.path, d.digest) | d <- digests])

-- | A hash representing the content of a source file at an earlier time, stored in the worker's incremental state.
newtype StoredHash =
  StoredHash { hash :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON, IsString)

-- | The worker's incremental state file, recording hashes from the previous successful run.
--
-- This is written by the worker after each metadata step and read on the next run to determine
-- which sources changed.
--
-- | Also stores the 'BuildPlanJson' from the previous run, so that unchanged modules'
-- build plan data can be carried forward without re-parsing their source.
data IncrementalState =
  IncrementalState {
    hashes :: Map OsPath StoredHash,
    buildPlanJson :: BuildPlanJson
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Convert the abstract 'SourceHash' values into 'StoredHash', for mild type safety.
storeHashes :: SourceHashes -> BuildPlanJson -> IncrementalState
storeHashes hashes buildPlanJson =
  IncrementalState {hashes = coerce hashes, ..}

data SourceChanges =
  SourceChanges {
    -- | Sources that need to be processed, because they were modified or added.
    updated :: Set OsPath,
    -- | Sources whose cached data may not be reused, because they were modified or removed.
    invalidated :: Set OsPath
  }
  deriving stock (Eq, Show)

-- | Produce two sets of source paths:
--
-- - Source paths present in the current unit, mapped to their stored hash as a 'Maybe', indicating that they were
--   modified or added.
-- - Source paths absent from the current unit and present in the stored paths, indicating that the files were removed.
classifyPresence ::
  Map OsPath StoredHash ->
  Set OsPath ->
  (Map OsPath (Maybe StoredHash), Set OsPath)
classifyPresence previous targets =
  Map.keysSet <$> Map.mapEither id changes
  where
    changes = Map.merge targetMissing prevMissing prevIsTarget previous (Map.fromSet (const ()) targets)

    -- There is a stored hash, but the path isn't part of the targets, so the file was removed.
    targetMissing = mapMissing \ _ _ -> Right ()

    -- A target has no corresponding stored hash, so the file was added.
    prevMissing = mapMissing \ _ _ -> Left Nothing

    -- A target has a corresponding stored hash.
    prevIsTarget = zipWithMatched \ _ s _ -> Left (Just s)

-- | Determine which source paths were modified and which were added.
-- The left return value consists of only the modified paths, while the right value combines modified and added paths.
compareHashes ::
  Map OsPath SourceHash ->
  Map OsPath (Maybe StoredHash) ->
  (Set OsPath, Set OsPath)
compareHashes sourceHashes storedHashes =
  (Map.keysSet (Map.filter id updated), Map.keysSet updated)
  where
    updated = Map.merge currentMissing currentNotTarget (zipWithMaybeMatched checkHash) sourceHashes storedHashes

    -- The source hashes are missing an entry for a target.
    -- Unclear how that would realistically happen, but we just treat it an indicator that the target requires
    -- reprocessing.
    currentMissing = mapMissing \ _ _ -> False

    -- The source hashes contain an entry for a file that isn't a target of the current request.
    -- If 'sourceHashesForTargets' was used to filter the hashes, this won't happen.
    currentNotTarget = dropMissing

    checkHash _ source = \case
      Just stored ->
        if stored.hash == source.hash
        then Nothing
        else Just True
      Nothing -> Just False

-- | Compare stored hashes from a previous build, the current build's source hashes, and the current build's explicit
-- targets, to determine the changes that require either:
--
-- - Computing their build plan entries, if they were modified or added
-- - Invalidating their cached build plan entries, if they were modified or removed
--
-- In principle, we could ignore @targets@ and assume that the source hashes contain exactly those files, but we assume
-- that @targets@ is the most authoritative source, to avoid any reproducibility issues.
sourceChanges ::
  IncrementalState ->
  SourceHashes ->
  Set OsPath ->
  SourceChanges
sourceChanges IncrementalState {hashes = stored} (SourceHashes sources) targets =
  SourceChanges {
    updated,
    invalidated = modified <> removed
  }
  where
    (modified, updated) = compareHashes sources present

    (present, removed) = classifyPresence stored targets
