module Ghc.Ui.Utils where

import Brick (Widget)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (centerLayer)
import Brick.Widgets.Core (hLimitPercent, str, vLimitPercent)
import Data.Sequence qualified as Seq
import Ghc.Ui.Data.Name (Name)

popup :: Int -> String -> Widget Name -> Widget Name
popup size popupTitle content =
  centerLayer $
    hLimitPercent size $
      vLimitPercent size $
        borderWithLabel (str $ " " ++ popupTitle ++ " ") content

upsertAscSeq :: (Ord b) => (a -> b) -> a -> Seq.Seq a -> (Int, Seq.Seq a)
upsertAscSeq meas x as = binSearch 0 (Seq.length as - 1)
 where
  binSearch l r
    | l > r = (l, Seq.insertAt l x as)
    | otherwise =
        let m = (l + r) `div` 2
            x' = Seq.index as m
            b' = meas x'
         in if meas x < b'
              then binSearch l (m - 1)
              else
                if meas x > b'
                  then binSearch (m + 1) r
                  else (m, Seq.update m x as)
