{- This file was auto-generated from instrument.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Instrument_Fields where
import qualified Prelude
import qualified Data.Int
import qualified Data.Monoid
import qualified Data.Word
import qualified Data.ProtoLens
import qualified Data.ProtoLens.Encoding.Bytes
import qualified Data.ProtoLens.Encoding.Growing
import qualified Data.ProtoLens.Encoding.Parser.Unsafe
import qualified Data.ProtoLens.Encoding.Wire
import qualified Data.ProtoLens.Field
import qualified Data.ProtoLens.Message.Enum
import qualified Data.ProtoLens.Service.Types
import qualified Lens.Family2
import qualified Lens.Family2.Unchecked
import qualified Data.Text
import qualified Data.Map
import qualified Data.ByteString
import qualified Data.ByteString.Char8
import qualified Data.Text.Encoding
import qualified Data.Vector
import qualified Data.Vector.Generic
import qualified Data.Vector.Unboxed
import qualified Text.Read
canDebug ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "canDebug" a) =>
  Lens.Family2.LensLike' f s a
canDebug = Data.ProtoLens.Field.field @"canDebug"
compileEnd ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "compileEnd" a) =>
  Lens.Family2.LensLike' f s a
compileEnd = Data.ProtoLens.Field.field @"compileEnd"
compileStart ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "compileStart" a) =>
  Lens.Family2.LensLike' f s a
compileStart = Data.ProtoLens.Field.field @"compileStart"
cpuNs ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "cpuNs" a) =>
  Lens.Family2.LensLike' f s a
cpuNs = Data.ProtoLens.Field.field @"cpuNs"
exitCode ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "exitCode" a) =>
  Lens.Family2.LensLike' f s a
exitCode = Data.ProtoLens.Field.field @"exitCode"
extraGhcOptions ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "extraGhcOptions" a) =>
  Lens.Family2.LensLike' f s a
extraGhcOptions = Data.ProtoLens.Field.field @"extraGhcOptions"
gcCpuNs ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "gcCpuNs" a) =>
  Lens.Family2.LensLike' f s a
gcCpuNs = Data.ProtoLens.Field.field @"gcCpuNs"
halt ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "halt" a) =>
  Lens.Family2.LensLike' f s a
halt = Data.ProtoLens.Field.field @"halt"
key ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "key" a) =>
  Lens.Family2.LensLike' f s a
key = Data.ProtoLens.Field.field @"key"
maybe'compileEnd ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'compileEnd" a) =>
  Lens.Family2.LensLike' f s a
maybe'compileEnd = Data.ProtoLens.Field.field @"maybe'compileEnd"
maybe'compileStart ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'compileStart" a) =>
  Lens.Family2.LensLike' f s a
maybe'compileStart
  = Data.ProtoLens.Field.field @"maybe'compileStart"
maybe'event ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'event" a) =>
  Lens.Family2.LensLike' f s a
maybe'event = Data.ProtoLens.Field.field @"maybe'event"
maybe'halt ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'halt" a) =>
  Lens.Family2.LensLike' f s a
maybe'halt = Data.ProtoLens.Field.field @"maybe'halt"
maybe'stats ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'stats" a) =>
  Lens.Family2.LensLike' f s a
maybe'stats = Data.ProtoLens.Field.field @"maybe'stats"
memory ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "memory" a) =>
  Lens.Family2.LensLike' f s a
memory = Data.ProtoLens.Field.field @"memory"
stats ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "stats" a) =>
  Lens.Family2.LensLike' f s a
stats = Data.ProtoLens.Field.field @"stats"
stderr ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "stderr" a) =>
  Lens.Family2.LensLike' f s a
stderr = Data.ProtoLens.Field.field @"stderr"
target ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "target" a) =>
  Lens.Family2.LensLike' f s a
target = Data.ProtoLens.Field.field @"target"
value ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "value" a) =>
  Lens.Family2.LensLike' f s a
value = Data.ProtoLens.Field.field @"value"