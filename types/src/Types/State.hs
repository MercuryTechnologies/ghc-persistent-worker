module Types.State where

import GHC.Data.FastString (FastString)
import GHC.Ptr (Ptr)
import GHC.Types.Unique.FM (UniqFM)

type SymbolMap = UniqFM FastString (Ptr ())

newtype SymbolCache =
  SymbolCache { symbols :: SymbolMap }
  deriving newtype (Semigroup, Monoid)
