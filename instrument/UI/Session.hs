{-# LANGUAGE TemplateHaskell #-}

module UI.Session where

import Brick.Types (EventM, Widget)
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Core (str, vBox, vLimitPercent)
import Control.Monad.IO.Class (liftIO)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Time (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Internal.Cache (Options (..))
import Lens.Micro.Platform (each, filtered, makeLenses, modifying, use, zoom)
import Network.GRPC.Common.Protobuf (Proto, (^.))
import Proto.Instrument qualified as Instr
import Proto.Instrument_Fields qualified as Instr
import UI.ActiveTasks qualified as ActiveTasks
import UI.ModuleSelector qualified as ModuleSelector
import UI.Types (Name)
import UI.Utils (formatBytes, formatPs, stripEscSeqs)

newtype Id = Id { unId :: Text.Text }
  deriving stock (Eq, Ord, Show)
newtype WorkerId = WorkerId { unWorkerId :: Text.Text }
  deriving stock (Eq, Ord, Show)

data State = Session
  { _title :: String
  , _workers :: [Worker]
  , _activeTasks :: ActiveTasks.State
  , _modules :: ModuleSelector.State
  , _sesStartTime :: UTCTime
  , _sesEndTime :: Maybe UTCTime
  , _finishedWorkerStats :: Stats
  }

data Worker = Worker
  { _workerId :: WorkerId
  , _sendOptions :: Options -> IO ()
  , _stats :: Stats
  }

data Stats = Stats
  { _memory :: Map.Map Text.Text Int -- in bytes
  , _gc_cpu_ns :: Int
  , _cpu_ns :: Int
  }
instance Semigroup Stats where
  Stats m1 gc1 cpu1 <> Stats m2 gc2 cpu2 =
    Stats (Map.unionWith (+) m1 m2) (gc1 + gc2) (cpu1 + cpu2)
instance Monoid Stats where
  mempty = Stats mempty 0 0

makeLenses ''State
makeLenses ''Worker
makeLenses ''Stats

data Event
  = InstrEvent WorkerId (Proto Instr.Event)

mkSession :: String -> UTCTime -> State
mkSession _title _startTime =
  Session
    { _title
    , _workers = []
    , _activeTasks = ActiveTasks.initialState
    , _modules = ModuleSelector.initialState
    , _sesStartTime = _startTime
    , _sesEndTime = Nothing
    , _finishedWorkerStats = mempty
    }

draw :: Name -> UTCTime -> State -> Widget Name
draw current now Session{..} =
  borderWithLabel (str $ " GHC Persistent Worker  " ++ _title ++ " ") $
    vBox
      [ vLimitPercent 30 $ ActiveTasks.draw current now _activeTasks
      , hBorder
      , ModuleSelector.draw current _modules
      , hBorder
      , drawStats (length _workers) (foldMap _stats _workers <> _finishedWorkerStats)
      ]

drawStats :: Int -> Stats -> Widget Name
drawStats workerCount Stats{..} =
  vBox
    [ str $
        " Worker count: "
          ++ show workerCount
          ++ " | Memory:"
          ++ concatMap
            (\(k, v) -> " " ++ Text.unpack k ++ "=" ++ formatBytes v)
            (Map.toList _memory)
    , str $
        " CPU Time: "
          ++ formatPs (1000 * _cpu_ns)
          ++ " | GC Time: "
          ++ formatPs (1000 * _gc_cpu_ns)
    ]

handleEvent :: Event -> EventM Name State ()
handleEvent (InstrEvent wid evt) =
  case evt ^. Instr.maybe'compileStart of
    Just cs -> do
      zoom activeTasks $ ActiveTasks.addTask (Text.unpack $ cs ^. Instr.target)
    _ -> case evt ^. Instr.maybe'compileEnd of
      Just ce -> do
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
          modifying (workers . each . filtered (\w -> w._workerId == wid) . stats) \st ->
            st
              { _memory = fromIntegral <$> msg ^. Instr.memory
              , _gc_cpu_ns = fromIntegral $ msg ^. Instr.gcCpuNs
              , _cpu_ns = fromIntegral $ msg ^. Instr.cpuNs
              }
        _ -> pure ()

removeWorker :: WorkerId -> EventM Name State ()
removeWorker wid = do
  st <- use (workers . each . filtered (\w -> w._workerId == wid) . stats)
  modifying finishedWorkerStats (<> st{_memory = mempty})
  modifying workers (filter (\w -> w._workerId /= wid))
