{-# LANGUAGE DeriveAnyClass #-}

module Types.CachedDeps where

import Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..), withArray, withText)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC (ModuleName, mkModuleNameFS)
import GHC.Data.FastString (mkFastStringByteString)
import GHC.Generics (Generic)

-- | JSON key wrapper.
newtype DepName =
  DepName ModuleName
  deriving stock (Eq, Show, Ord)

depNameFromText :: Text -> DepName
depNameFromText =
  DepName . mkModuleNameFS . mkFastStringByteString . encodeUtf8

instance FromJSONKey DepName where
  fromJSONKey = FromJSONKeyText depNameFromText
  fromJSONKeyList =
    FromJSONKeyValue (withArray "DepName" (traverse (withText "DepName" (pure . depNameFromText)) . toList))

-- | The data Buck provides in order to restore the state when recompiling after restart.
data CachedDeps =
  CachedDeps {
    local :: Map DepName (NonEmpty FilePath)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)
