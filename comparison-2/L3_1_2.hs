{-# language TemplateHaskell #-}
  module L3_1_2 where

import L2_1_1
import L2_1_2
import L2_2_1
import L2_2_2
import Dep1
import Dep2
import Dep3
import Dep4
import Dep5
import Dep6
import Dep7
import Dep8
import Dep9
import Dep10

use_1_2 :: Int
use_1_2 = 2 + 0 + $(l2_1_1_1) + $(l2_1_1_2) + $(l2_1_1_3) + $(l2_1_1_4) + $(l2_1_2_1) + $(l2_1_2_2) + $(l2_1_2_3) + $(l2_1_2_4) + $(l2_2_1_1) + $(l2_2_1_2) + $(l2_2_1_3) + $(l2_2_1_4) + $(l2_2_2_1) + $(l2_2_2_2) + $(l2_2_2_3) + $(l2_2_2_4) + 0 + $(dep1_1) + $(dep1_2) + $(dep1_3) + $(dep1_4) + $(dep2_1) + $(dep2_2) + $(dep2_3) + $(dep2_4) + $(dep3_1) + $(dep3_2) + $(dep3_3) + $(dep3_4) + $(dep4_1) + $(dep4_2) + $(dep4_3) + $(dep4_4) + $(dep5_1) + $(dep5_2) + $(dep5_3) + $(dep5_4) + $(dep6_1) + $(dep6_2) + $(dep6_3) + $(dep6_4) + $(dep7_1) + $(dep7_2) + $(dep7_3) + $(dep7_4) + $(dep8_1) + $(dep8_2) + $(dep8_3) + $(dep8_4) + $(dep9_1) + $(dep9_2) + $(dep9_3) + $(dep9_4) + $(dep10_1) + $(dep10_2) + $(dep10_3) + $(dep10_4)

