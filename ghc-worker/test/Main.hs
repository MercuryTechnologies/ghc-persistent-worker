module Main where

import CompileHptTest (test_compileHpt)
import Test.Tasty (TestTree, defaultMain, testGroup)
import UnitIndexTest (test_unitIndex)

tests :: TestTree
tests =
  testGroup "all" [
    test_compileHpt,
    test_unitIndex
  ]

main :: IO ()
main = defaultMain tests
