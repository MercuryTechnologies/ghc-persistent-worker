module Test.Data.Env where

import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import System.OsPath (OsPath)
import Test.Tasty (TestTree, askOption)
import Test.Tasty.Options (IsOption (..), OptionDescription (..), safeRead)
import Types.Args (Args)
import Types.Env (Env)

newtype MaxUnits =
  MaxUnits Int
  deriving stock (Eq, Show, Typeable)
  deriving newtype (Num, Real, Enum, Integral, Ord)

instance IsOption MaxUnits where
  defaultValue = 5
  parseValue = fmap MaxUnits . safeRead
  optionName = pure "max-units"
  optionHelp = pure "Maximum number of units in the generated project"

newtype MaxModulesPerUnit =
  MaxModulesPerUnit Int
  deriving stock (Eq, Show, Typeable)
  deriving newtype (Num, Real, Enum, Integral, Ord)

instance IsOption MaxModulesPerUnit where
  defaultValue = 5
  parseValue = fmap MaxModulesPerUnit . safeRead
  optionName = pure "max-modules-per-unit"
  optionHelp = pure "Maximum number of modules per unit in the generated project"

newtype MaxJobs =
  MaxJobs Int
  deriving stock (Eq, Show, Typeable)
  deriving newtype (Num, Real, Enum, Integral, Ord)

instance IsOption MaxJobs where
  defaultValue = 6
  parseValue = fmap MaxJobs . safeRead
  optionName = pure "max-concurrent-jobs"
  optionHelp = pure "Maximum number of concurrent build jobs in the scheduler"

data TestConfig =
  TestConfig {
    maxUnits :: MaxUnits,
    maxModulesPerUnit :: MaxModulesPerUnit,
    maxConcurrentJobs :: MaxJobs
  }
  deriving stock (Show)

testConfigOptions :: [OptionDescription]
testConfigOptions =
  [
    Option (Proxy @MaxUnits),
    Option (Proxy @MaxModulesPerUnit),
    Option (Proxy @MaxJobs)
  ]

withTestConfig :: (TestConfig -> TestTree) -> TestTree
withTestConfig use =
  askOption \ maxUnits ->
    askOption \ maxModulesPerUnit ->
      askOption \ maxConcurrentJobs ->
        use TestConfig {..}

data TestEnv =
  TestEnv {
    -- | Root temp dir.
    rootDir :: OsPath,
    -- | Empty worker args that contain the GHC distribution directory (@topdir@).
    baseArgs :: Args
  }

-- | Environment for a single GHC session with fresh temp directories for sources and outputs, as well as the basic
-- worker 'Env' with a fresh 'WorkerState'.
data SessionEnv =
  SessionEnv {
    shared :: TestEnv,
    sourceDir :: OsPath,
    tempDir :: OsPath,
    env :: Env
  }
