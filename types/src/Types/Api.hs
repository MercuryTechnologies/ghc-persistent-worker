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

data Event
  = CompileStart { target :: String, canDebug :: Bool }
  | CompileEnd { target :: String, exitCode :: Int, stderr :: String }
  | Stats { memory :: Map String Int, cpuNs :: Int, gcCpuNs :: Int }
  | Halt
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)
