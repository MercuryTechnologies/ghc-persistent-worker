{-# LANGUAGE ApplicativeDo #-}

module GhcWorker.Run where

import BuckWorkerProto (Instrument, Worker)
import Common.Grpc (GrpcHandler (..), fromGrpcHandler)
import Control.Applicative (many, optional, (<|>))
import Control.Concurrent (MVar, newChan, newMVar)
import Control.Concurrent.Chan (Chan)
import Data.Functor (void)
import GhcWorker.GhcHandler (ghcHandler)
import GhcWorker.Grpc (instrumentMethods)
import GhcWorker.Instrumentation (WorkerStatus (..), toGrpcHandler)
import GhcWorker.Orchestration (CreateMethods (..), runCentralGhcSpawned)
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
import Types.Grpc (CommandEnv, RequestArgs)
import Types.Instrument (Event)
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

    flagOption value = do
      flag <- eitherReader \case
        "fixed-nodes-cache" -> Right FeatureFixedNodesCache
        "flag-parser" -> Right FeatureFlagParser
        "concurrent-init-units" -> Right FeatureConcurrentInitUnits
        "instrument" -> Right FeatureInstrument
        "lazy-byte-code" -> Right FeatureLazyByteCode
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

-- | Allocate a communication channel for instrumentation events and construct a gRPC server handler that streams said
-- events to a client.
--
-- Returns the channel so that a GHC server can use it to send events.
createInstrumentMethods ::
  MVar WorkerState ->
  (CommandEnv -> RequestArgs -> IO ()) ->
  IO (Chan Event, Methods IO (ProtobufMethodsOf Instrument))
createInstrumentMethods stateVar recompile = do
  instrChan <- newChan
  pure (instrChan, instrumentMethods instrChan stateVar recompile)

-- | Construct a gRPC server handler for the main part of the persistent worker.
createGhcMethods ::
  MVar WorkerState ->
  FeatureFlags ->
  MVar WorkerStatus ->
  Maybe TraceId ->
  Maybe (Chan Event) ->
  IO (CommandEnv -> RequestArgs -> IO (), Methods IO (ProtobufMethodsOf Worker))
createGhcMethods state features status traceId instrChan =
  let handler = toGrpcHandler (ghcHandler state features traceId) status state instrChan
      voidRun commandEnv requestArgs =
        void $ handler.run commandEnv requestArgs
  in pure (voidRun, fromGrpcHandler handler)

-- | Main function for running the default persistent worker using the provided server socket path and CLI options.
runWorker :: CliOptions -> IO ()
runWorker CliOptions {serve, features} = do
  state <- newState
  status <- newMVar WorkerStatus {active = 0}
  let
    methods = CreateMethods {
      createInstrumentation = createInstrumentMethods state,
      createGhc = createGhcMethods state features status traceId
    }
  runCentralGhcSpawned methods features serve
  where
    traceId = if null serve.traceId then Nothing else Just (TraceId serve.traceId)

parseCliArgs :: IO CliOptions
parseCliArgs = execParser cliOptionsParserInfo
