{-# LANGUAGE DeriveAnyClass #-}

module Types.BuildPlan where

import Control.Applicative ((<|>))
import Data.Aeson (ToJSON (..), Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import GHC.Generics (Generic)
import GHC.Unit.Module (IsBootInterface (..), ModuleName (..), UnitId (..))
import GHC.Unit.Module.Graph (ModuleGraph)
import Types.CachedDeps (JsonFs (..))

data Dep =
  Dep {
    name :: ModuleName,
    unit :: UnitId,
    boot :: IsBootInterface
  }

data PackageDep =
  PackageDep {
    id :: JsonFs UnitId,
    name :: String,
    modules :: [String]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

newtype Preprocessor =
  Preprocessor (Maybe String)
  deriving stock (Eq, Show)
  deriving newtype (ToJSON)

data BuildPlanModule =
  BuildPlanModule {
    sources :: NonEmpty FilePath,
    modules :: [JsonFs ModuleName],
    modulesBoot :: [JsonFs ModuleName],
    packages :: [PackageDep],
    cpp :: [FilePath],
    options :: [String],
    preprocessor :: Preprocessor
  }
  deriving stock (Eq, Show)

instance ToJSON BuildPlanModule where
  toJSON BuildPlanModule {..} =
    object [
      "sources" .= toJSON sources,
      "modules" .= toJSON modules,
      "modules-boot" .= toJSON modulesBoot,
      "packages" .= toJSON packages,
      "cpp" .= toJSON cpp,
      "options" .= toJSON options,
      "preprocessor" .= toJSON preprocessor
    ]

data BuildPlanEntry =
  BuildPlanEntry {
    regular :: Maybe BuildPlanModule,
    boot :: Maybe BuildPlanModule
  }
  deriving stock (Eq, Show)

combineBuildPlanEntries :: BuildPlanEntry -> BuildPlanEntry -> BuildPlanEntry
combineBuildPlanEntries BuildPlanEntry {regular, boot} BuildPlanEntry {regular = regular', boot = boot'} =
  BuildPlanEntry {regular = regular <|> regular', boot = boot <|> boot'}

instance ToJSON BuildPlanEntry where
  toJSON BuildPlanEntry {..} =
    case toJSON regular of
      Object values | Just bootData <- boot ->
        Object (KeyMap.insert "boot" (toJSON bootData) values)
      value -> value

data BuildPlan =
  BuildPlan {
    graph :: ModuleGraph,
    modules :: Map (JsonFs ModuleName) BuildPlanEntry
  }
