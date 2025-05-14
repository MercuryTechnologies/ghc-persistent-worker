{-# language TemplateHaskell #-}
module L3_2 where

import L2_1
import Dep1

use_2 :: Int
use_2 = 2 + $(l2_1) + $(dep1_1)
