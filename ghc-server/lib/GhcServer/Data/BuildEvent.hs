-- | Events logged during the build pipeline for observability and test assertions.
--
-- Each constructor represents a conditional action — something that happens depending on
-- build state (cache presence, unit request type, etc.).  Static procedure steps (e.g.
-- "started scheduler loop") are not logged because they always happen and carry no
-- diagnostic value.
module GhcServer.Data.BuildEvent where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import GHC (ModuleName, moduleNameString)
import GhcServer.Data.Unit (UnitName (..))

-- | A build event representing a conditional action in the pipeline.
data BuildEvent =
  -- | Metadata was skipped because the unit's cache exists.
  MetadataSkipped UnitName
  |
  -- | Metadata was run (fresh downsweep).
  MetadataRan UnitName
  |
  -- | A module was compiled.
  ModuleCompiled UnitName ModuleName
  |
  -- | A module's compilation was skipped because cached artifacts exist.
  CompileSkipped UnitName ModuleName
  |
  -- | Resolution was computed from cache data after metadata.
  ResolutionComputed UnitName
  deriving stock (Eq)

instance Show BuildEvent where
  show = \case
    MetadataSkipped name -> "MetadataSkipped " ++ name.string
    MetadataRan name -> "MetadataRan " ++ name.string
    ModuleCompiled name modName -> "ModuleCompiled " ++ name.string ++ ":" ++ moduleNameString modName
    CompileSkipped name modName -> "CompileSkipped " ++ name.string ++ ":" ++ moduleNameString modName
    ResolutionComputed name -> "ResolutionComputed " ++ name.string

-- | Mutable event log for recording build events.
newtype BuildEvents =
  BuildEvents { ref :: IORef [BuildEvent] }

-- | Create a new empty event log.
newBuildEvents :: IO BuildEvents
newBuildEvents =
  BuildEvents <$> newIORef []

-- | Record a build event (appends to the end in order).
logEvent :: BuildEvents -> BuildEvent -> IO ()
logEvent events event =
  atomicModifyIORef' events.ref \ es -> (event : es, ())

-- | Read all recorded events in chronological order.
readEvents :: BuildEvents -> IO [BuildEvent]
readEvents events =
  reverse <$> readIORef events.ref
