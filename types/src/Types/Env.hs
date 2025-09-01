module Types.Env where

import Control.Concurrent (MVar)
import Types.Args (Args)
import Types.Log (Logger)
import Types.State (WorkerState)

-- | Data used by a single worker request session, consisting of a logger, shared state, and request arguments.
data Env =
  Env {
    -- | Logger used to receive messages from GHC and relay them to Buck.
    log :: Logger,

    -- | The entirety of the persistent state of a worker that's shared across sessions.
    state :: MVar WorkerState,

    -- | Preprocessed command line args from Buck.
    args :: Args
  }
