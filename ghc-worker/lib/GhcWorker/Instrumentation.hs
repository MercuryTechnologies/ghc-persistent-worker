module GhcWorker.Instrumentation where

import Common.Grpc (GrpcHandler (..))
import Control.Concurrent (MVar, modifyMVar_, readMVar)
import Control.Concurrent.Chan (Chan, writeChan)
import Control.Exception (bracket_)
import Data.Foldable (traverse_)
import Data.Int (Int32)
import Data.Text qualified as Text
import GhcWorker.Grpc (mkStats)
import Internal.State (WorkerState)
import Internal.Log (dbg)
import Network.GRPC.Common.Protobuf (Proto, defMessage, (&), (.~))
import Prelude hiding (log)
import qualified Proto.Instrument as Instr
import Proto.Instrument_Fields qualified as Instr
import Types.State (Target (..))

-- | Rudimentary dummy state for instrumentation, counting concurrently compiling sessions.
data WorkerStatus =
  WorkerStatus {
    active :: Int
  }

-- | Callbacks passed to GHC request handlers that trigger instrumentation events.
data Hooks =
  Hooks {
    -- | A module compilation is started.
    -- If it can be determined at this point, the argument contains the file name.
    -- This is not available in multiplexer mode.
    compileStart :: Maybe Target -> IO (),

    -- | A module compilation has finished.
    -- If the job was successful, the argument contains 'Just' the stderr lines and the exit code, otherwise 'Nothing'.
    compileFinish :: Maybe (Maybe Target, [String], Int32) -> IO ()
  }

-- | Dummy implementation of 'Hooks'.
hooksNoop :: Hooks
hooksNoop =
  Hooks {
    compileStart = const (pure ()),
    compileFinish = const (pure ())
  }

-- | A request handler that is aware of instrumentation.
newtype InstrumentedHandler =
  InstrumentedHandler { create :: Hooks -> GrpcHandler }

-- | Register a newly started job by incrementing the active job count.
startJob ::
  MVar WorkerStatus ->
  IO ()
startJob var =
  modifyMVar_ var \ ws@WorkerStatus {active} -> do
    let new = active + 1
    dbg ("Starting job, now " ++ show new ++ " active")
    pure ws {active = new}

-- | Decrement the active job count.
finishJob ::
  MVar WorkerStatus ->
  IO ()
finishJob var = do
  modifyMVar_ var \ ws@WorkerStatus {active} -> do
    let new = active - 1
    dbg ("Finishing job, now " ++ show new ++ " active")
    pure ws {active = new}

-- | Construct a grapesy message for a "compilation started" event.
messageCompileStart :: String -> Proto Instr.CompileStart
messageCompileStart target =
  defMessage
    & Instr.target .~ Text.pack target

-- | Construct a grapesy message for a "compilation finished" event.
messageCompileEnd :: String -> Int -> String -> Proto Instr.CompileEnd
messageCompileEnd target exitCode err =
  defMessage
    & Instr.target .~ Text.pack target
    & Instr.exitCode .~ fromIntegral exitCode
    & Instr.stderr .~ Text.pack err

-- | Run a 'GrpcHandler' with instrumentation enabled.
--
-- This consists of adapting the active job count and sending messages to the gRPC client running the instrumentation
-- app.
-- The handler is initialized by passing 'Hooks' to its constructor function, which contains callbacks for sending
-- additional messages.
withInstrumentation ::
  Chan (Proto Instr.Event) ->
  MVar WorkerStatus ->
  MVar WorkerState ->
  InstrumentedHandler ->
  GrpcHandler
withInstrumentation instrChan status stateVar handler =
  GrpcHandler \ commandEnv argv -> do
    state <- readMVar stateVar
    bracket_ (startJob status) (finishJob status) do
      result <- (handler.create hooks).run commandEnv argv
      stats <- mkStats state
      writeChan instrChan (defMessage & Instr.stats .~ stats)
      pure result
  where
    hooks = Hooks {
      compileStart,
      compileFinish
    }

    compileStart =
      traverse_ \ target ->
        writeChan instrChan $
          defMessage &
            Instr.compileStart .~
              messageCompileStart target.path

    -- Note: This is WIP.
    compileFinish =
      traverse_ \ (target, output, exitCode) -> do
        let tgt = maybe "" (.path) target
        writeChan instrChan $
          defMessage &
            Instr.compileEnd .~
              messageCompileEnd tgt (fromIntegral exitCode) (unlines output)

-- | Construct a 'GrpcHandler' by passing functioning 'Hooks' to an 'InstrumentedHandler' if the third argument contains
-- 'Just' a message channel, or passing no-op 'Hooks' otherwise.
toGrpcHandler ::
  InstrumentedHandler ->
  MVar WorkerStatus ->
  MVar WorkerState ->
  Maybe (Chan (Proto Instr.Event)) ->
  GrpcHandler
toGrpcHandler createHandler status stateVar = \case
  Nothing -> createHandler.create hooksNoop
  Just instrChan -> withInstrumentation instrChan status stateVar createHandler
