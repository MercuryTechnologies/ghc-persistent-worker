{-# OPTIONS_GHC -Wno-orphans #-}

module BuckWorker (module Proto.Worker) where

import Network.GRPC.Common (
  NoMetadata,
  RequestMetadata,
  ResponseInitialMetadata,
  ResponseTrailingMetadata,
 )
import Network.GRPC.Common.Protobuf (Protobuf)
import Proto.Worker

type instance RequestMetadata (Protobuf Worker "exec") = NoMetadata
type instance ResponseInitialMetadata (Protobuf Worker "exec") = NoMetadata
type instance ResponseTrailingMetadata (Protobuf Worker "exec") = NoMetadata

type instance RequestMetadata (Protobuf Worker "execute") = NoMetadata
type instance ResponseInitialMetadata (Protobuf Worker "execute") = NoMetadata
type instance ResponseTrailingMetadata (Protobuf Worker "execute") = NoMetadata
