module Ghc.Ui.Render.Session where

import Brick.Types (Widget)
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Core (str, vBox, vLimitPercent)
import Data.Generics.Labels ()
import Data.Map qualified as Map
import Data.Time (UTCTime)
import Ghc.Ui.Data.Name (Name)
import Ghc.Ui.Data.Session (SessionState (..), Stats (..), Worker (..))
import Ghc.Ui.ModuleSelector qualified as ModuleSelector
import Ghc.Ui.Tasks qualified as Tasks
import Ghc.Ui.Utils (formatBytes, formatPs)

drawStats :: Int -> Stats -> Widget Name
drawStats workerCount Stats{..} =
  vBox
    [ str $
        " Worker count: "
          ++ show workerCount
          ++ " | Memory:"
          ++ concatMap
            (\(k, v) -> " " ++ k ++ "=" ++ formatBytes v)
            (Map.toList memory)
    , str $
        " CPU Time: "
          ++ formatPs (1000 * cpu_ns)
          ++ " | GC Time: "
          ++ formatPs (1000 * gc_cpu_ns)
    ]

draw :: Name -> UTCTime -> SessionState -> Widget Name
draw current now SessionState {..} =
  borderWithLabel (str $ " GHC Persistent Worker  " ++ title ++ " ") $
    vBox
      [ vLimitPercent 30 $ Tasks.draw current now activeTasks
      , hBorder
      , ModuleSelector.draw current modules
      , hBorder
      , drawStats (length workers) (foldMap (.stats) workers <> finishedWorkerStats)
      ]
