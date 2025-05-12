module Types.Grpc where

import Data.Map (Map)

-- | The environment variables sent by Buck.
newtype CommandEnv =
  CommandEnv (Map String String)
  deriving stock (Eq, Show)

-- | The command line arguments sent by Buck.
newtype RequestArgs =
  RequestArgs [String]
  deriving stock (Eq, Show)
