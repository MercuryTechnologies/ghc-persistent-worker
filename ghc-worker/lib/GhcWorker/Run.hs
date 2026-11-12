{-# LANGUAGE ApplicativeDo #-}

module GhcWorker.Run where

import BuckWorkerProto (Worker)
import Control.Applicative (many, optional, (<|>))
import Control.Concurrent (MVar, newChan, newMVar)
import GhcWorker.GhcHandler (ghcHandler)
import GhcWorker.Grpc (ghcMethods)
import GhcWorker.Instrumentation (WorkerStatus (..), toGrpcHandler)
import GhcWorker.Orchestration (runCentralGhcSpawned)
import Internal.State (newState)
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (Methods)
import Options.Applicative (
  Parser,
  ParserInfo,
  eitherReader,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  progDesc,
  strOption,
  (<**>),
  )
import Types.FeatureFlags (FeatureFlag (..), FeatureFlags (..), defaultFeatureFlags, parseByteSize)
import Types.Log (TraceId (..))
import Types.Orchestration (ServerSocketPath (..), serverSocketFromPath)
import Types.State (WorkerState (..))
import System.OsPath.Extra (toOsPath)

-- | Global options for the worker, passed when the process is started, in contrast to request options stored in
-- 'BuckArgs'.
data CliOptions =
  CliOptions {
    -- | If this is given, the app should start a GHC server synchronously, listening on the given path.
    serve :: ServerSocketPath,

    -- | Runtime feature flags.
    features :: FeatureFlags
  }
  deriving stock (Eq, Show)

-- | Parser for runtime feature flags.
featureFlagsParser :: Parser FeatureFlags
featureFlagsParser =
  applyFlags <$> many (
    (option (flagOption True) (long "enable" <> metavar "FEATURE" <> help "Enable an optional feature"))
    <|>
    (option (flagOption False) (long "disable" <> metavar "FEATURE" <> help "Disable an optional feature"))
    )
  where
    applyFlags =
      flip foldl' defaultFeatureFlags \ flags -> \case
        (fixedNodesCache, FeatureFixedNodesCache) -> flags {fixedNodesCache}
        (flagParser, FeatureFlagParser) -> flags {flagParser}
        (concurrentInitUnits, FeatureConcurrentInitUnits) -> flags {concurrentInitUnits}
        (instrument, FeatureInstrument) -> flags {instrument}
        (lazyByteCode, FeatureLazyByteCode) -> flags {lazyByteCode}
        (useIncrModGraph, FeatureIncrModGraph) -> flags {useIncrModGraph}

    flagOption value = do
      flag <- eitherReader \case
        "fixed-nodes-cache" -> Right FeatureFixedNodesCache
        "flag-parser" -> Right FeatureFlagParser
        "concurrent-init-units" -> Right FeatureConcurrentInitUnits
        "instrument" -> Right FeatureInstrument
        "lazy-byte-code" -> Right FeatureLazyByteCode
        "incr-mod-graph" -> Right FeatureIncrModGraph
        flag -> Left ("Invalid feature flag: " ++ flag)
      pure (value, flag)

-- | Parser for '--max-bytecode', bounding the loader state size.
maxBytecodeParser :: Parser (Maybe Int)
maxBytecodeParser =
  optional (
    option (eitherReader parseByteSize) (
      long "max-bytecode"
      <> metavar "NUM[k|M|G]"
      <> help "Maximum number of BCOs in the loader state (enables eviction)"
      )
    )

cliOptionsParser :: Parser CliOptions
cliOptionsParser = do
  serve <- serverSocketFromPath . toOsPath <$> strOption (long "serve" <> metavar "SOCKET" <> help "Socket path for the GHC server")
  features0 <- featureFlagsParser
  maxBytecode <- maxBytecodeParser
  pure CliOptions {serve, features = features0 {lazyByteCodeCacheLimit = maxBytecode}}

cliOptionsParserInfo :: ParserInfo CliOptions
cliOptionsParserInfo =
  info (cliOptionsParser <**> helper)
    (fullDesc <> progDesc "GHC persistent worker" <> header "ghc-worker")

-- | Construct the @worker@ service for the persistent worker.
-- Instrumentation is only active if the feature flag is enabled.
createMethods ::
  MVar WorkerState ->
  FeatureFlags ->
  MVar WorkerStatus ->
  Maybe TraceId ->
  IO (Methods IO (ProtobufMethodsOf Worker))
createMethods state features status traceId = do
  instrChan <- if features.instrument then Just <$> newChan else pure Nothing
  pure (ghcMethods state instrChan (toGrpcHandler (ghcHandler state features traceId) status state instrChan))

-- | Main function for running the default persistent worker using the provided server socket path and CLI options.
runWorker :: CliOptions -> IO ()
runWorker CliOptions {serve, features} = do
  state <- newState
  status <- newMVar WorkerStatus {active = 0, nextRequestId = 0}
  methods <- createMethods state features status traceId
  runCentralGhcSpawned methods serve
  where
    traceId = if null serve.traceId then Nothing else Just (TraceId serve.traceId)

parseCliArgs :: IO CliOptions
parseCliArgs = execParser cliOptionsParserInfo
