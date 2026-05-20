module Types.Target where

import Data.String (IsString (fromString))
import GHC (Module, moduleName, moduleNameString)
import GHC.Unit (UnitId, moduleUnitId, unitIdString)
import GHC.Utils.Outputable (Outputable (..), showPprUnsafe, text, (<+>))
import System.OsPath.Extra (OsPath, fromOsPath)

-- | The path to the source file the worker is currently compiling.
-- used primarily to index maps in the state and for logging.
newtype Target =
  Target { path :: OsPath }
  deriving stock (Eq, Show)
  deriving newtype (Ord)

instance Outputable Target where
  ppr (Target path) = text (fromOsPath path)

newtype ModuleTarget =
  ModuleTarget { mod :: Module }
  deriving stock (Eq)
  deriving newtype (Outputable)

instance Show ModuleTarget where
  show (ModuleTarget m) = showPprUnsafe m

newtype UnitTarget =
  UnitTarget { unit :: UnitId }
  deriving stock (Eq)
  deriving newtype (Outputable)

instance Show UnitTarget where
  show (UnitTarget unit) = showPprUnsafe unit

data TargetSpec =
  TargetSource Target
  |
  TargetModule ModuleTarget
  |
  TargetModuleInterp ModuleTarget
  |
  TargetUnit UnitTarget
  |
  TargetUnknown String
  deriving stock (Eq, Show)

renderTargetSpec :: IsString a => TargetSpec -> a
renderTargetSpec = \case
  TargetSource (Target path) -> fromString $ fromOsPath path
  TargetModule (ModuleTarget m) -> fromString (unitIdString (moduleUnitId m) ++ ":" ++ moduleNameString (moduleName m))
  TargetModuleInterp (ModuleTarget m) -> fromString (unitIdString (moduleUnitId m) ++ ":" ++ moduleNameString (moduleName m) ++ ":<interpreted>")
  TargetUnit (UnitTarget unit) -> fromString (unitIdString unit)
  TargetUnknown spec -> fromString spec

instance Ord TargetSpec where
  compare l r = compare (renderTargetSpec @String l) (renderTargetSpec r)

instance Outputable TargetSpec where
  ppr = \case
    TargetSource (Target path) -> text (fromOsPath path)
    TargetModule (ModuleTarget m) -> ppr m
    TargetModuleInterp (ModuleTarget m) -> ppr m <+> text "<interp>"
    TargetUnit (UnitTarget unit) -> ppr unit
    TargetUnknown spec -> text spec
