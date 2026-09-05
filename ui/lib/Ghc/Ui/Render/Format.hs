module Ghc.Ui.Render.Format where

import Data.Fixed (Fixed (..), Pico)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)

format :: (Integral a, Show a) => NonEmpty String -> a -> String
format (unit :| units) n
  | n >= 10_000
  , Just rest <- nonEmpty units
  = format rest (n `div` 1_000)
  | otherwise
  = show n ++ unit

formatBytes :: (Integral a, Show a) => a -> String
formatBytes = format ["b", "Kb", "Mb", "Gb", "Tb", "Pb"]

formatPs :: (Integral a, Show a) => a -> String
formatPs = format ["ps", "ns", "µs", "ms", "s"]

formatPico :: Pico -> String
formatPico (MkFixed n) = formatPs n
