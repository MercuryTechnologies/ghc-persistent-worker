{-# language TemplateHaskell #-}
module L3_3 where

import L3_2
import L2_1
import Dep1

use_3 :: Int
use_3 = use_2 + 3 + $(l2_1) + $(dep1_1)
