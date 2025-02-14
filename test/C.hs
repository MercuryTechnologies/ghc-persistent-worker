{-# LANGUAGE TemplateHaskell #-}
module C where

import A
import B

test1 :: IO ()
test1 = $(test [|"C"|])

test3 :: IO ()
test3 = do
  test1
  test2