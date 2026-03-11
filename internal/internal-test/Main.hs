module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "all" [
  ]

main :: IO ()
main = defaultMain tests
