module Main where

import CompileHptTest (test_compileHpt)
import ProjectBuildTest (test_projectBuild)
import ScheduleTest (test_sortScheduleOrder)
import Test.Data.Env (testConfigOptions)
import Test.Tasty (TestTree, defaultMainWithIngredients, defaultIngredients, includingOptions, testGroup)
import UnitIndexTest (test_unitIndex)

tests :: TestTree
tests =
  testGroup "all" [
    test_compileHpt,
    test_sortScheduleOrder,
    test_projectBuild,
    test_unitIndex
  ]

main :: IO ()
main =
  defaultMainWithIngredients (includingOptions testConfigOptions : defaultIngredients) tests
