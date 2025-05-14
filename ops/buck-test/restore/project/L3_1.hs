{-# language TemplateHaskell #-}
module L3_1 where

import L2_1
import Dep1

use_1 :: Int
use_1 = 1 + 0 + $(l2_1) + 0 + $(dep1_1)
