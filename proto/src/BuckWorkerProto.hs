{-# OPTIONS_GHC -Wno-orphans #-}

module BuckWorkerProto (module Proto.Worker) where

import Network.GRPC.Common (
  NoMetadata,
  RequestMetadata,
  ResponseInitialMetadata,
  ResponseTrailingMetadata,
 )
import Network.GRPC.Common.Protobuf (Protobuf)
import Proto.Worker

type instance RequestMetadata (Protobuf Worker _) = NoMetadata
type instance ResponseInitialMetadata (Protobuf Worker _) = NoMetadata
type instance ResponseTrailingMetadata (Protobuf Worker _) = NoMetadata
