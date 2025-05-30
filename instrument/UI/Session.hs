{-# LANGUAGE TemplateHaskell #-}

module UI.Session where

import Brick.Types (EventM, Widget)
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Core (str, vBox, vLimitPercent)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Time (UTCTime, getCurrentTime, nominalDiffTimeToSeconds, diffUTCTime)
import Internal.Cache (Options (..))
import Lens.Micro.Platform (makeLenses, modifying, zoom)
import Network.GRPC.Common.Protobuf (Proto, (^.))
import Proto.Instrument qualified as Instr
import Proto.Instrument_Fields qualified as Instr
import UI.ActiveTasks qualified as ActiveTasks
import UI.ModuleSelector qualified as ModuleSelector
import UI.Types (Name)
import UI.Utils (formatBytes, formatPs, stripEscSeqs)
import Control.Monad.IO.Class (liftIO)

type Id = FilePath

data State = Session
  { _title :: String
  , _activeTasks :: ActiveTasks.State
  , _modules :: ModuleSelector.State
  , _sesStartTime :: UTCTime
  , _sesEndTime :: Maybe UTCTime
  , _stats :: Stats
  , _sendOptions :: Options -> IO ()
  }

data Stats = Stats
  { _memory :: Map.Map Text.Text Int -- in bytes
  , _gc_cpu_ns :: Int
  , _cpu_ns :: Int
  , _activeTaskCount :: Int
  }

makeLenses ''State
makeLenses ''Stats

data Event
  = InstrEvent (Proto Instr.Event)

mkSession :: String -> UTCTime -> (Options -> IO ()) -> State
mkSession _title _startTime _sendOptions =
  Session
    { _title
    , _activeTasks = ActiveTasks.initialState
    , _modules = ModuleSelector.initialState
    , _sesStartTime = _startTime
    , _sesEndTime = Nothing
    , _stats = Stats mempty 0 0 0
    , _sendOptions
    }

draw :: Name -> UTCTime -> State -> Widget Name
draw current now Session{..} =
  borderWithLabel (str $ " GHC Persistent Worker  " ++ _title ++ " ") $
    vBox
      [ vLimitPercent 30 $ ActiveTasks.draw current now _activeTasks
      , hBorder
      , ModuleSelector.draw current _modules
      , hBorder
      , drawStats _stats
      ]

drawStats :: Stats -> Widget Name
drawStats Stats{..} =
  vBox
    [ str $
        "Memory:"
          ++ concatMap
            (\(k, v) -> " " ++ Text.unpack k ++ "=" ++ formatBytes v)
            (Map.toList _memory)
    , str $
        " CPU Time: "
          ++ formatPs (1000 * _cpu_ns)
          ++ " | GC Time: "
          ++ formatPs (1000 * _gc_cpu_ns)
          ++ (if _activeTaskCount > 0 then " | Active jobs: " ++ show _activeTaskCount else "")
    ]

handleEvent :: Event -> EventM Name State ()
handleEvent (InstrEvent evt) =
  case evt ^. Instr.maybe'compileStart of
    Just cs -> do
      zoom activeTasks $ ActiveTasks.addTask (Text.unpack $ cs ^. Instr.target)
      modifying (stats . activeTaskCount) succ
    _ -> case evt ^. Instr.maybe'compileEnd of
      Just ce -> do
        modifying (stats . activeTaskCount) pred
        let content = stripEscSeqs (Text.unpack $ ce ^. Instr.stderr)
            target' = Text.unpack $ ce ^. Instr.target
            target = if target' == "" then takeWhile (/= ':') content else target'
        if ce ^. Instr.exitCode == 0
          then do
            start <- zoom activeTasks $ ActiveTasks.removeTask target
            end <- liftIO getCurrentTime
            let time = nominalDiffTimeToSeconds . diffUTCTime end <$> start
            zoom modules $ ModuleSelector.addModule target content time
          else do
            zoom activeTasks $ ActiveTasks.taskFailure target content
      _ -> case evt ^. Instr.maybe'stats of
        Just msg -> do
          modifying stats \st ->
            st
              { _memory = fromIntegral <$> msg ^. Instr.memory
              , _gc_cpu_ns = fromIntegral $ msg ^. Instr.gcCpuNs
              , _cpu_ns = fromIntegral $ msg ^. Instr.cpuNs
              }
        _ -> pure ()