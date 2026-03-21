module Test.Data.SourceMode where

import Test.Data.Project (ModuleKey)

-- | Flag that determines the content of a module's source file that's written to disk before each build.
-- Resume builds only write the sources selected for modification.
--
-- The variable content is the RHS of a binding, which uses the bindings defined in each of its dependencies.
data SourceMode =
  -- | Write the default expression, which additionally depends on whether the module is selected to contain an error.
  SourceNormal
  |
  -- | Update the source file, requiring it to be rebuilt.
  -- Since the test simulates a build system, it explicitly forces the rebuild independent of any modifications, but we
  -- want the source hash to be outdated to reproduce real-world circumstances.
  SourceModified
  |
  -- | The module contained an error in the initial build and will be fixed in the resume build.
  SourceFixed
  deriving stock (Eq, Ord, Show)

-- | A source file rewrite: the @mode@ determines the expression, and @deps@ provides the full dependency list,
-- including any added deps from resume mutations.
data SourceRewrite =
  SourceRewrite {
    mode :: SourceMode,
    deps :: [ModuleKey],
    -- | Whether to generate a Template Haskell splice expression.
    th :: Bool,
    -- | Number of top-level value bindings to generate.
    bindings :: Int
  }
  deriving stock (Eq, Show)

-- | The data needed to write a module's source file.
data ModuleSource =
  ModuleSource {
    deps :: [ModuleKey],
    -- | Whether to generate a Template Haskell splice expression.
    th :: Bool,
    -- | Number of top-level value bindings to generate.
    bindings :: Int
  }
  deriving stock (Eq, Show)
