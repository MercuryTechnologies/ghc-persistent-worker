{-# LANGUAGE TemplateHaskell #-}
module B where

import A

test2 :: IO ()
test2 = $(test [|"B"|])