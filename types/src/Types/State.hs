module Types.State where

import Data.String (IsString (fromString))
import GHC (Module, moduleName, moduleNameString)
import GHC.Data.FastString (FastString)
import GHC.Ptr (Ptr)
import GHC.Types.Unique.FM (UniqFM)
import GHC.Unit (UnitId, moduleUnitId, unitIdString)
import GHC.Utils.Outputable (Outputable (..), showPprUnsafe, text)

-- | The path to the source file the worker is currently compiling.
-- used primarily to index maps in the state and for logging.
newtype Target =
  Target { path :: FilePath }
  deriving stock (Eq, Show)
  deriving newtype (Ord)

instance Outputable Target where
  ppr (Target path) = text path

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
  TargetUnit UnitTarget
  |
  TargetUnknown String
  deriving stock (Eq, Show)

renderTargetSpec :: IsString a => TargetSpec -> a
renderTargetSpec = \case
  TargetSource (Target path) -> fromString path
  TargetModule (ModuleTarget m) -> fromString (unitIdString (moduleUnitId m) ++ ":" ++ moduleNameString (moduleName m))
  TargetUnit (UnitTarget unit) -> fromString (unitIdString unit)
  TargetUnknown spec -> fromString spec

instance Ord TargetSpec where
  compare l r = compare (renderTargetSpec @String l) (renderTargetSpec r)

instance Outputable TargetSpec where
  ppr = \case
    TargetSource (Target path) -> text path
    TargetModule (ModuleTarget m) -> ppr m
    TargetUnit (UnitTarget unit) -> ppr unit
    TargetUnknown spec -> text spec

type SymbolMap = UniqFM FastString (Ptr ())

newtype SymbolCache =
  SymbolCache { symbols :: SymbolMap }
  deriving newtype (Semigroup, Monoid)
