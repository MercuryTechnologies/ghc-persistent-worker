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
exitCode ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "exitCode" a) =>
  Lens.Family2.LensLike' f s a
exitCode = Data.ProtoLens.Field.field @"exitCode"
halt ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "halt" a) =>
  Lens.Family2.LensLike' f s a
halt = Data.ProtoLens.Field.field @"halt"
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