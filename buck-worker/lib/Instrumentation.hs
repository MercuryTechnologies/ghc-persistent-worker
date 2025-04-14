module Instrumentation where

import Control.Concurrent (MVar, modifyMVar_, writeChan)
import Control.Concurrent.Chan (Chan)
import Control.Exception (bracket_)
import Data.Foldable (traverse_)
import Data.Int (Int32)
import Data.Text qualified as Text
import Grpc (GrpcHandler (..), mkStats)
import Internal.Cache (Target (..))
import Internal.Log (dbg)
import Network.GRPC.Common.Protobuf (Proto, defMessage, (&), (.~))
import Prelude hiding (log)
import qualified Proto.Instrument as Instr
import Proto.Instrument_Fields qualified as Instr

data WorkerStatus =
  WorkerStatus {
    active :: Int
  }

data Hooks =
  Hooks {
    compileStart :: Maybe Target -> IO (),
    compileFinish :: Maybe ([String], Int32) -> IO ()
  }

hooksNoop :: Hooks
hooksNoop =
  Hooks {
    compileStart = const (pure ()),
    compileFinish = const (pure ())
  }

newtype InstrumentedHandler =
  InstrumentedHandler { create :: Hooks -> GrpcHandler }

startJob ::
  MVar WorkerStatus ->
  IO ()
startJob var =
  modifyMVar_ var \ ws@WorkerStatus {active} -> do
    let new = active + 1
    dbg ("Starting job, now " ++ show new ++ " active")
    pure ws {active = new}

finishJob ::
  MVar WorkerStatus ->
  IO ()
finishJob var = do
  modifyMVar_ var \ ws@WorkerStatus {active} -> do
    let new = active - 1
    dbg ("Finishing job, now " ++ show new ++ " active")
    pure ws {active = new}

messageCompileStart :: String -> Proto Instr.CompileStart
messageCompileStart target =
  defMessage
    & Instr.target .~ Text.pack target

messageCompileEnd :: String -> Int -> String -> Proto Instr.CompileEnd
messageCompileEnd target exitCode err =
  defMessage
    & Instr.target .~ Text.pack target
    & Instr.exitCode .~ fromIntegral exitCode
    & Instr.stderr .~ Text.pack err

withInstrumentation ::
  Chan (Proto Instr.Event) ->
  MVar WorkerStatus ->
  InstrumentedHandler ->
  GrpcHandler
withInstrumentation instrChan status handler =
  GrpcHandler \ commandEnv argv ->
    bracket_ (startJob status) (finishJob status) do
      result <- (handler.create hooks).run commandEnv argv
      stats <- mkStats
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
              messageCompileStart target.get

    compileFinish =
      traverse_ \ (output, exitCode) ->
        writeChan instrChan $
          defMessage &
            Instr.compileEnd .~
              messageCompileEnd "" (fromIntegral exitCode) (unlines output)

toGrpcHandler ::
  InstrumentedHandler ->
  MVar WorkerStatus ->
  Maybe (Chan (Proto Instr.Event)) ->
  GrpcHandler
toGrpcHandler createHandler status = \case
  Nothing -> createHandler.create hooksNoop
  Just instrChan -> withInstrumentation instrChan status createHandler
