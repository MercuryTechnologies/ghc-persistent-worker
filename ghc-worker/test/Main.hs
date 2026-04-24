module Main where

import BuildPlanTest (test_buildPlan_make, test_buildPlan_oneshot)
import CompileHptTest (test_compileHpt)
import ProjectBuildTest (test_projectBuild)
import ScheduleTest (test_sortScheduleOrder)
import Test.Data.Env (testConfigOptions)
import Test.Tasty (TestTree, defaultIngredients, defaultMainWithIngredients, includingOptions, testGroup)

tests :: TestTree
tests =
  testGroup "all" [
    test_buildPlan_make,
    test_buildPlan_oneshot,
    test_compileHpt,
    test_sortScheduleOrder,
    test_projectBuild
  ]

main :: IO ()
main =
  defaultMainWithIngredients (includingOptions testConfigOptions : defaultIngredients) tests
