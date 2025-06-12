{-# OPTIONS_GHC -Wno-orphans #-}

module BuckWorker (module Proto.Worker, module Proto.Instrument) where

import Network.GRPC.Common (
  NoMetadata,
  RequestMetadata,
  ResponseInitialMetadata,
  ResponseTrailingMetadata,
 )
import Network.GRPC.Common.Protobuf (Protobuf)
import Proto.Worker
import Proto.Instrument

type instance RequestMetadata (Protobuf Worker _) = NoMetadata
type instance ResponseInitialMetadata (Protobuf Worker _) = NoMetadata
type instance ResponseTrailingMetadata (Protobuf Worker _) = NoMetadata

type instance RequestMetadata (Protobuf Instrument _) = NoMetadata
type instance ResponseInitialMetadata (Protobuf Instrument _) = NoMetadata
type instance ResponseTrailingMetadata (Protobuf Instrument _) = NoMetadata
