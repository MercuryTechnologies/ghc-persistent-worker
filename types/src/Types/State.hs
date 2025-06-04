module Types.State where

import GHC.Data.FastString (FastString)
import GHC.Ptr (Ptr)
import GHC.Types.Unique.FM (UniqFM)

-- | The path to the source file the worker is currently compiling.
-- used primarily to index maps in the state and for logging.
newtype Target =
  Target { path :: FilePath }
  deriving stock (Eq, Show)
  deriving newtype (Ord)

type SymbolMap = UniqFM FastString (Ptr ())

newtype SymbolCache =
  SymbolCache { symbols :: SymbolMap }
  deriving newtype (Semigroup, Monoid)
