{-# LANGUAGE NoFieldSelectors #-}

module Types.Api where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Map (Map)
import Data.String (IsString)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified GHC
import GHC.Generics (Generic)
import GHC.Unit (Module, UnitId, mkModuleName, moduleName, moduleNameString, moduleUnitId, stringToUnitId, unitIdString)
import qualified Types.Target as Worker
import Types.Target (ModuleTarget (..), TargetSpec, UnitTarget (..))

newtype UnitName =
  UnitName { text :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, Binary, FromJSON, ToJSON)

fromUnitId :: UnitId -> UnitName
fromUnitId name =
  UnitName (Text.pack (unitIdString name))

toUnitId :: UnitName -> UnitId
toUnitId name =
  stringToUnitId (Text.unpack name.text)

newtype ModuleName =
  ModuleName { text :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, Binary, FromJSON, ToJSON)

fromGhcModuleName :: GHC.ModuleName -> ModuleName
fromGhcModuleName name =
  ModuleName (Text.pack (moduleNameString name))

toGhcModuleName :: ModuleName -> GHC.ModuleName
toGhcModuleName name =
  mkModuleName (Text.unpack name.text)

data HomeModule =
  HomeModule {
    unit :: UnitName,
    name :: ModuleName
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Binary, FromJSON, ToJSON)

homeModuleFromGhc :: Module -> HomeModule
homeModuleFromGhc module_ =
  HomeModule {
    unit = fromUnitId (moduleUnitId module_),
    name = fromGhcModuleName (moduleName module_)
  }

data UnitSummary =
  UnitSummary {
    name :: UnitName,
    modules :: [ModuleName]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | Specification for selecting build targets by names, especially from the UI.
data Target =
  TargetProject
  |
  TargetUnit { name :: UnitName }
  |
  TargetModule { key :: HomeModule }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Binary, FromJSON, ToJSON)

homeModuleMatchTarget :: HomeModule -> Target -> Bool
homeModuleMatchTarget candidate = \case
  TargetProject -> True
  TargetUnit {name} -> candidate.unit == name
  TargetModule {key} -> candidate == key

renderTarget :: Target -> Text
renderTarget = \case
  TargetProject -> "the project"
  TargetUnit {name = UnitName name} -> name
  TargetModule {key = HomeModule {unit = UnitName unit, name = ModuleName name}} -> unit <> ":" <> name

-- | Whether the second target is included in the first.
targetContains :: Target -> Target -> Bool
targetContains = \cases
  TargetProject _ -> True
  TargetUnit {name} TargetUnit {name = candidate} -> name == candidate
  TargetUnit {name} TargetModule {key = candidate} -> name == candidate.unit
  reference candidate -> reference == candidate

targetFromWorkerSpec :: TargetSpec -> Maybe Target
targetFromWorkerSpec = \case
  Worker.TargetModule ModuleTarget {module_} -> Just TargetModule {key = homeModuleFromGhc module_}
  Worker.TargetModuleInterp ModuleTarget {module_} -> Just TargetModule {key = homeModuleFromGhc module_}
  Worker.TargetUnit UnitTarget {unit} -> Just TargetUnit {name = UnitName (Text.pack (unitIdString unit))}
  _ -> Nothing

data TaskKind =
  Metadata
  |
  Build { rebuild :: Bool }
  |
  Execute
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary, FromJSON, ToJSON)

data TaskTrigger =
  TaskTrigger {
    target :: Target,
    task :: TaskKind
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary, FromJSON, ToJSON)

data ApiRequest =
  TriggerTask TaskTrigger
  |
  EvictBytecode Target
  |
  Clean Target
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ApiResponse =
  ApiSuccess
  |
  ApiFailure { message :: Text }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Event
  = CompileStart { target :: String, canDebug :: Bool }
  | CompileEnd { target :: String, exitCode :: Int, stderr :: String }
  | Stats { memory :: Map String Int, cpuNs :: Int, gcCpuNs :: Int }
  | Halt
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)
