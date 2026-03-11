module Internal.Env where

import Control.Concurrent (MVar)
import Control.Exception (finally)
import Internal.Log (logFlushDebug, newLogger)
import Types.Args (Args)
import Types.Env (Env (..))
import Types.Log (newLog)
import Types.State (WorkerState)

-- | Run a program with a fresh log and print all messages to stderr afterwards.
withDebugLog :: MVar WorkerState -> Args -> (Env -> IO a) -> IO a
withDebugLog state args use = do
  logState <- newLog Nothing
  let env = Env {log = newLogger logState, ..}
  finally (use env) (logFlushDebug logState)
