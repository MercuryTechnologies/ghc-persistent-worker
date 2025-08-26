module UI.Utils where

import Brick.Types (EventM, Widget)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (centerLayer)
import Brick.Widgets.Core (hLimitPercent, str, vLimitPercent)
import Brick.Widgets.List (GenericList, Splittable, handleListEvent, handleListEventVi)
import Data.Fixed (Fixed (..), Pico)
import Data.Sequence qualified as Seq
import Graphics.Vty qualified as V
import Lens.Micro.Platform (Traversal', zoom)
import UI.Types (Name)

popup :: Int -> String -> Widget Name -> Widget Name
popup size popupTitle content =
  centerLayer $
    hLimitPercent size $
      vLimitPercent size $
        borderWithLabel (str $ " " ++ popupTitle ++ " ") content

formatBytes :: (Integral a, Show a) => a -> String
formatBytes = format ["b", "Kb", "Mb", "Gb", "Tb", "Pb"]

formatPs :: (Integral a, Show a) => a -> String
formatPs = format ["ps", "ns", "µs", "ms", "s"]

formatPico :: Pico -> String
formatPico (MkFixed n) = formatPs n

format :: (Integral a, Show a) => [String] -> a -> String
format [unit] n = show n ++ unit
format (unit : units) n
  | n >= 10_000 = format units (n `div` 1_000)
  | otherwise = show n ++ unit
format [] _ = error "No units given"

stripEscSeqs :: String -> String
stripEscSeqs [] = []
stripEscSeqs ('\ESC' : '[' : xs) = stripEscSeqs (drop 1 (dropWhile (/= 'm') xs))
stripEscSeqs (x : xs) = x : stripEscSeqs xs

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

handleListEventOf :: (Foldable t, Splittable t, Ord n) => Traversal' s (GenericList n t e) -> V.Event -> EventM n s ()
handleListEventOf lens = zoom lens . handleListEventVi handleListEvent