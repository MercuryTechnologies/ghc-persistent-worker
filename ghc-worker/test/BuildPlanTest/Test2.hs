{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

--
-- Try to make build plan file for an example with missing deps.
-- The test should show all the errors.
--
module BuildPlanTest.Test2 where

import Control.Monad.Catch (MonadCatch (catch))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (ByteString, pack)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC (DynFlags (..), Ghc, GhcMode (..), Target (..))
import GHC.Driver.Env (hscUpdateFlags)
import GHC.Driver.Monad (modifySession)
import GHC.Types.Error (Messages (..))
import GHC.Types.SourceError (SourceError (..))
import GHC.Unit (UnitId, stringToUnitId)
import Hedgehog (forAll, property, withTests, (===))
import qualified Hedgehog.Gen as Gen (int)
import Hedgehog.Range (linear)
import Internal.BuildPlan (buildPlanForTargets)
import Internal.Log (newLogger)
import Prelude hiding (log)
import System.FilePath ((</>))
import Test.PackageDb (ModuleSpec (..), UnitSpec (..), moduleSpec)
import Test.Run (transientSession, withTemp)
import Test.Target (fileUnitTargets, ghcOptions)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Types.Args (BuildPlanField (..), buildPlanAll)
import Types.BuildPlan (BuildPlan (..))
import Types.Log (Logger, newLog)

unit1 :: UnitId
unit1 = stringToUnitId "unit1"

unit1Modules :: Int -> NonEmpty ModuleSpec
unit1Modules numMissingImports =
  let missingImports :: [ByteString]
      missingImports =  map (\n -> "import NonExistent" <> pack (show n)) [1..numMissingImports]
      srcTxt =
        ["module U1M1 where"] ++
        missingImports ++
        [
          "u1m1 :: Int",
          "u1m1 = 5"
        ]
   in [moduleSpec "U1M1" srcTxt]

unit1Spec :: Int -> UnitSpec
unit1Spec numMissingImports =
  UnitSpec {name = "unit1", deps = [], modules = unit1Modules numMissingImports}

fields :: Set BuildPlanField
fields = Set.fromList (toList buildPlanAll)

runBuildPlan :: Logger -> NonEmpty Target -> Ghc BuildPlan
runBuildPlan logger targets = do
  modifySession (hscUpdateFlags \ d -> d {ghcMode = MkDepend})
  buildPlanForTargets logger fields mempty mempty (toList targets)

-- | Run build plan JSON generation and should get errors from non-existent module imports
--   The number of errors should be matched with the number of errorneous imports.
test_buildPlan :: TestTree
test_buildPlan =
  withTemp "build-plan-make" \ tmpResource -> do
    testProperty "build plan import errors" $ withTests 10 $ property do
      numMissingImports <- forAll $ Gen.int (linear 1 10)
      tmp <- liftIO tmpResource
      targets <- liftIO $ fileUnitTargets (tmp </> "src") (unit1Spec numMissingImports)
      log <- liftIO $ newLog Nothing
      let logger = newLogger log
      numErrs <- transientSession (ghcOptions unit1 []) do
        catch (runBuildPlan logger targets >> pure 0) \(SourceError msg) -> do
          let errs = toList (getMessages msg)
          pure (length errs)
      numMissingImports === numErrs
