{-# LANGUAGE CPP #-}

module Main where
 
import BuildPlanTest (test_buildPlan)
import ProjectBuildTest (test_projectBuild)
import ResourceTest (test_resources)
import ScheduleTest (test_sortScheduleOrder)
import Test.Data.Env (testConfigOptions)
import Test.Tasty (
  DependencyType (AllFinish),
  TestTree,
  after,
  defaultIngredients,
  defaultMainWithIngredients,
  includingOptions,
  testGroup,
  )

-- | Some tests require our GHC patches.
fullTest :: Bool

#if defined(MWB) || defined(MWB_2025_10)

fullTest = True

#else

fullTest = False

#endif

testsGeneral :: [TestTree]
testsGeneral =
  [
    test_sortScheduleOrder,
    test_projectBuild
  ] <> if fullTest then [
    test_buildPlan
  ] else []

tests :: TestTree
tests =
  testGroup "all" [
    test_resources,
    afterResources (testGroup "general" testsGeneral)
  ]
  where
    -- tasty 1.5 has @sequentialTestGroup@, but the current Nix env has 1.4, so we'll make do with this for now.
    afterResources = after AllFinish "resources"

main :: IO ()
main =
  defaultMainWithIngredients (includingOptions testConfigOptions : defaultIngredients) tests
