module L2_1 where

import L1_1
import L4_1
import L4_2

import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Syntax (lift)

l2_1 :: ExpQ
l2_1 = lift @_ @Int (1 + 0 + l1_1 + l4_1 + l4_2)
