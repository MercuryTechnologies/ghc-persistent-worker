module Ghc.Ui.Data.ServerApi where

import Network.GRPC.Client (Connection)
import Types.State (Options)
import Types.Target (TargetSpec)

-- | Abstraction of gRPC endpoints.
-- This is crucial to keep out of the event handler logic, because the implementation brings instances of @IsLabel@ from
-- @proto-lens@ into scope, which clash with @generic-lens@.
data ServerApi =
  ServerApi {
    sendOptions :: Connection -> Options -> IO (),
    triggerRebuild :: Connection -> TargetSpec -> IO ()
  }
