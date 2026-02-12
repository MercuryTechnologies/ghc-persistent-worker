module Test.Data.TestLog where

import GHC (Severity)
import GHC.Types.Error (DiagnosticCode)
import Types.Target (TargetSpec)

-- | Data used for printing and asserting a GHC diagnostic.
data DiagnosticEntry =
  DiagnosticEntry {
    code :: Maybe DiagnosticCode,
    severity :: Severity,
    rendered :: String
  }

-- | Logger state used for testing that stores more details used for assertions at the end.
data TestLog =
  TestLog {
    target :: Maybe TargetSpec,
    diagnostics :: [DiagnosticEntry],
    fatal :: [String],
    messages :: [String]
  }
