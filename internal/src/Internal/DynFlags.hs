module Internal.DynFlags where

import Control.Monad (unless)
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import qualified GHC
import GHC (DynFlags (..), GhcException (..), GhcLink (LinkBinary), Phase, parseDynamicFlags, parseTargetFiles)
import GHC.Driver.Config.Diagnostic (initDiagOpts, initPrintConfig)
import GHC.Driver.Config.Logger (initLogFlags)
import GHC.Driver.Errors (printOrThrowDiagnostics)
import GHC.Driver.Errors.Types (DriverMessages, GhcMessage (GhcDriverMessage))
import GHC.Types.SrcLoc (Located, mkGeneralLocated, unLoc)
import GHC.Utils.Logger (setLogFlags)
import GHC.Utils.Panic (throwGhcExceptionIO)
import Prelude hiding (log)
import System.Environment (setEnv)
import Types.State (BinPath (..), WorkerState (..))

-- | Add all the directories passed by Buck in @--bin-path@ options to the global @$PATH@.
-- Although Buck intends these to be module specific, all subsequent compile jobs will see all previous jobs' entries,
-- since we only have one process environment.
setupPath :: [String] -> WorkerState -> IO WorkerState
setupPath binPath old = do
  setEnv "PATH" (intercalate ":" (toList path.extra ++ maybeToList path.initial))
  pure new
  where
    path = new.path
    new = old {path = old.path {extra}}
    extra
      | Just cur <- nonEmpty binPath
      = Set.union old.path.extra (Set.fromList (toList cur))
      | otherwise
      = old.path.extra

buckLocation :: a -> Located a
buckLocation = mkGeneralLocated "by Buck2"

instrumentLocation :: a -> Located a
instrumentLocation = mkGeneralLocated "by instrument"

-- | Parse command line flags into @DynFlags@ and set up the logger. Extracted from GHC.
-- Returns the subset of args that have not been recognized as options.
parseFlags ::
  DynFlags ->
  GHC.Logger ->
  [Located String] ->
  IO (DynFlags, GHC.Logger, [Located String], DriverMessages)
parseFlags dflags0 logger0 argv = do
  let dflags1 = dflags0 {ghcLink = LinkBinary, verbosity = 0}
  let logger1 = setLogFlags logger0 (initLogFlags dflags1)
  (dflags, fileish_args, dynamicFlagWarnings) <- parseDynamicFlags logger1 dflags1 argv
  pure (dflags, setLogFlags logger1 (initLogFlags dflags), fileish_args, dynamicFlagWarnings)

-- | Parse CLI args and initialize 'DynFlags'.
-- Returns the subset of args that have not been recognized as options.
initDynFlags ::
  DynFlags ->
  GHC.Logger ->
  [Located String] ->
  DriverMessages ->
  IO (DynFlags, [(String, Maybe Phase)])
initDynFlags dflags0 logger fileish_args dynamicFlagWarnings = do
  printOrThrowDiagnostics logger (initPrintConfig dflags0) (initDiagOpts dflags0) flagWarnings'
  let (dflags1, srcs, objs) = parseTargetFiles dflags0 (map unLoc fileish_args)
  unless (null objs) $ throwGhcExceptionIO (UsageError ("Targets contain object files: " ++ show objs))
  pure (dflags1, srcs)
  where
    flagWarnings' = GhcDriverMessage <$> dynamicFlagWarnings
