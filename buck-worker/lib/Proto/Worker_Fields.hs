{- This file was auto-generated from worker.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Worker_Fields where
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
argv ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "argv" a) =>
  Lens.Family2.LensLike' f s a
argv = Data.ProtoLens.Field.field @"argv"
cancel ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "cancel" a) =>
  Lens.Family2.LensLike' f s a
cancel = Data.ProtoLens.Field.field @"cancel"
command ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "command" a) =>
  Lens.Family2.LensLike' f s a
command = Data.ProtoLens.Field.field @"command"
env ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "env" a) =>
  Lens.Family2.LensLike' f s a
env = Data.ProtoLens.Field.field @"env"
exitCode ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "exitCode" a) =>
  Lens.Family2.LensLike' f s a
exitCode = Data.ProtoLens.Field.field @"exitCode"
key ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "key" a) =>
  Lens.Family2.LensLike' f s a
key = Data.ProtoLens.Field.field @"key"
maybe'cancel ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'cancel" a) =>
  Lens.Family2.LensLike' f s a
maybe'cancel = Data.ProtoLens.Field.field @"maybe'cancel"
maybe'command ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'command" a) =>
  Lens.Family2.LensLike' f s a
maybe'command = Data.ProtoLens.Field.field @"maybe'command"
maybe'data' ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'data'" a) =>
  Lens.Family2.LensLike' f s a
maybe'data' = Data.ProtoLens.Field.field @"maybe'data'"
stderr ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "stderr" a) =>
  Lens.Family2.LensLike' f s a
stderr = Data.ProtoLens.Field.field @"stderr"
value ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "value" a) =>
  Lens.Family2.LensLike' f s a
value = Data.ProtoLens.Field.field @"value"
vec'argv ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'argv" a) =>
  Lens.Family2.LensLike' f s a
vec'argv = Data.ProtoLens.Field.field @"vec'argv"
vec'env ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "vec'env" a) =>
  Lens.Family2.LensLike' f s a
vec'env = Data.ProtoLens.Field.field @"vec'env"