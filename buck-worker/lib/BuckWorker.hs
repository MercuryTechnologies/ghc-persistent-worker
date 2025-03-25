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

type instance RequestMetadata (Protobuf Worker "exec") = NoMetadata
type instance ResponseInitialMetadata (Protobuf Worker "exec") = NoMetadata
type instance ResponseTrailingMetadata (Protobuf Worker "exec") = NoMetadata

type instance RequestMetadata (Protobuf Worker "execute") = NoMetadata
type instance ResponseInitialMetadata (Protobuf Worker "execute") = NoMetadata
type instance ResponseTrailingMetadata (Protobuf Worker "execute") = NoMetadata

type instance RequestMetadata (Protobuf Instrument "notifyMe") = NoMetadata
type instance ResponseInitialMetadata (Protobuf Instrument "notifyMe") = NoMetadata
type instance ResponseTrailingMetadata (Protobuf Instrument "notifyMe") = NoMetadata
