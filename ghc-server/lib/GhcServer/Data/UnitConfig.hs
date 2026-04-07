-- | JSON-serializable unit configuration read from @unit.json@.
module GhcServer.Data.UnitConfig where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

-- | The contents of a @unit.json@ file in a unit directory.
data UnitConfig =
  UnitConfig {
    -- | Names of home units that this unit depends on.
    deps :: [String],
    -- | GHC CLI arguments for this unit.
    args :: [String]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
