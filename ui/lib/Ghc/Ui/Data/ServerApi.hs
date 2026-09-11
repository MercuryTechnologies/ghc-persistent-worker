module Ghc.Ui.Data.ServerApi where

import qualified Ghc.Ui.Data.Session as Session
import Ghc.Ui.Data.WorkerId (WorkerId)
import Network.GRPC.Client (Connection)
import Types.Api (Target)

-- | Abstraction of gRPC endpoints.
-- This is crucial to keep out of the event handler logic, because the implementation brings instances of @IsLabel@ from
-- @proto-lens@ into scope, which clash with @generic-lens@.
data ServerApi =
  ServerApi {
    -- | Ask a worker to rebuild a target, in the background.
    -- A failure is reported to the session as 'Session.RebuildFailed'.
    triggerRebuild :: Session.Id -> WorkerId -> Connection -> Target -> IO ()
  }
