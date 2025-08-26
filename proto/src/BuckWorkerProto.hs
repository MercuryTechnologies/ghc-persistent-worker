{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module BuckWorkerProto (module Proto.Worker, module Proto.Instrument, module BuckWorkerProto) where

import Network.GRPC.Common (
  NoMetadata,
  RequestMetadata,
  ResponseInitialMetadata,
  ResponseTrailingMetadata,
 )
import Network.GRPC.Common.Protobuf (Protobuf, Proto)
import Proto.Worker
import Proto.Instrument

type instance RequestMetadata (Protobuf Worker _) = NoMetadata
type instance ResponseInitialMetadata (Protobuf Worker _) = NoMetadata
type instance ResponseTrailingMetadata (Protobuf Worker _) = NoMetadata

type instance RequestMetadata (Protobuf Instrument _) = NoMetadata
type instance ResponseInitialMetadata (Protobuf Instrument _) = NoMetadata
type instance ResponseTrailingMetadata (Protobuf Instrument _) = NoMetadata

pattern CompileStart :: Proto CompileStart -> Proto Event
pattern CompileStart cs <- ((.maybe'compileStart) -> Just cs)

pattern CompileEnd :: Proto CompileEnd -> Proto Event
pattern CompileEnd ce <- ((.maybe'compileEnd) -> Just ce)

pattern Stats :: Proto Stats -> Proto Event
pattern Stats msg <- ((.maybe'stats) -> Just msg)

pattern Halt :: Proto Event
pattern Halt <- ((.maybe'halt) -> Just _)

{-# COMPLETE CompileStart, CompileEnd, Stats, Halt #-}
