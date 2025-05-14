{-# language TemplateHaskell #-}
  module L3_1_1 where

import L2_1_1
import Dep1
import Dep2
import Dep3
import Dep4

use_1_1 :: Int
use_1_1 = 1 + 0 + $(l2_1_1_1) + $(l2_1_1_2) + $(l2_1_1_3) + $(l2_1_1_4) + 0 + $(dep1_1) + $(dep2_1) + $(dep3_1) + $(dep4_1)

