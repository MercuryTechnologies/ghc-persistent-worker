module GhcWorker.CompileResult where

import Data.Foldable (for_)
import Data.Int (Int32)
import Internal.AbiHash (AbiHash (..))
import Internal.State (ModuleArtifacts)
import Types.BuckArgs (BuckArgs (..))

-- | Right now the 'Maybe' just corresponds to the presence of the CLI argument @--abi-out@ – errors occuring while
-- reading the iface are thrown.
data CompileResult =
  CompileResult {
    artifacts :: ModuleArtifacts,
    abiHash :: Maybe AbiHash
  }
  deriving stock (Show)

writeResult :: BuckArgs -> Maybe CompileResult -> IO Int32
writeResult args = \case
  Nothing -> pure 1
  Just CompileResult {abiHash} -> do
    for_ abiHash \ AbiHash {path, hash} -> writeFile path hash
    for_ args.buck2Dep \ path -> writeFile path "\n"
    for_ args.buck2PackageDbDep \ path ->
      case args.buck2PackageDb of
        [] -> writeFile path "\n"
        dbs -> writeFile path (unlines dbs)
    pure 0

writeCloseOutput :: BuckArgs -> IO Int32
writeCloseOutput args =
  case args.closeOutput of
    Nothing -> pure 1
    Just path -> writeFile path "\n" >> pure 0
