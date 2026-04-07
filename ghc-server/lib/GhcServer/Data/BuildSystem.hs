-- | Domain-specific handler bundle for the build scheduler.
--
-- A concrete instantiation of the generic scheduler 'Handlers' with
-- the build system's key, value, and extension types.
module GhcServer.Data.BuildSystem where

import GhcServer.Build.Propagate (BuildExt)
import GhcServer.Build.Schedule (BuildStatus, TaskKey)
import GhcServer.Data.Request (ScheduleRequest)
import GhcServer.Scheduler (Handlers)

-- | Build-system handlers wired into the scheduler.
--
-- Specializes the generic 'Handlers' to the build system's types.
type BuildSystem = Handlers ScheduleRequest TaskKey BuildStatus BuildExt
