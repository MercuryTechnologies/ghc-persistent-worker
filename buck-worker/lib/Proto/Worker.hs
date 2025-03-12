{- This file was auto-generated from worker.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Worker (
        Worker(..), ExecuteCancel(), ExecuteCommand(),
        ExecuteCommand'EnvironmentEntry(), ExecuteEvent(),
        ExecuteEvent'Data(..), _ExecuteEvent'Command, _ExecuteEvent'Cancel,
        ExecuteResponse()
    ) where
import qualified Data.ProtoLens.Runtime.Control.DeepSeq as Control.DeepSeq
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Prism as Data.ProtoLens.Prism
import qualified Data.ProtoLens.Runtime.Prelude as Prelude
import qualified Data.ProtoLens.Runtime.Data.Int as Data.Int
import qualified Data.ProtoLens.Runtime.Data.Monoid as Data.Monoid
import qualified Data.ProtoLens.Runtime.Data.Word as Data.Word
import qualified Data.ProtoLens.Runtime.Data.ProtoLens as Data.ProtoLens
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Bytes as Data.ProtoLens.Encoding.Bytes
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Growing as Data.ProtoLens.Encoding.Growing
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Parser.Unsafe as Data.ProtoLens.Encoding.Parser.Unsafe
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Wire as Data.ProtoLens.Encoding.Wire
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Field as Data.ProtoLens.Field
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Message.Enum as Data.ProtoLens.Message.Enum
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Service.Types as Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Runtime.Lens.Family2 as Lens.Family2
import qualified Data.ProtoLens.Runtime.Lens.Family2.Unchecked as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Runtime.Data.Text as Data.Text
import qualified Data.ProtoLens.Runtime.Data.Map as Data.Map
import qualified Data.ProtoLens.Runtime.Data.ByteString as Data.ByteString
import qualified Data.ProtoLens.Runtime.Data.ByteString.Char8 as Data.ByteString.Char8
import qualified Data.ProtoLens.Runtime.Data.Text.Encoding as Data.Text.Encoding
import qualified Data.ProtoLens.Runtime.Data.Vector as Data.Vector
import qualified Data.ProtoLens.Runtime.Data.Vector.Generic as Data.Vector.Generic
import qualified Data.ProtoLens.Runtime.Data.Vector.Unboxed as Data.Vector.Unboxed
import qualified Data.ProtoLens.Runtime.Text.Read as Text.Read
{- | Fields :
      -}
data ExecuteCancel
  = ExecuteCancel'_constructor {_ExecuteCancel'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExecuteCancel where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message ExecuteCancel where
  messageName _ = Data.Text.pack "worker.ExecuteCancel"
  packedMessageDescriptor _
    = "\n\
      \\rExecuteCancel"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExecuteCancel'_unknownFields
        (\ x__ y__ -> x__ {_ExecuteCancel'_unknownFields = y__})
  defMessage
    = ExecuteCancel'_constructor {_ExecuteCancel'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExecuteCancel -> Data.ProtoLens.Encoding.Bytes.Parser ExecuteCancel
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ExecuteCancel"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData ExecuteCancel where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq (_ExecuteCancel'_unknownFields x__) ()
{- | Fields :
     
         * 'Proto.Worker_Fields.argv' @:: Lens' ExecuteCommand [Data.ByteString.ByteString]@
         * 'Proto.Worker_Fields.vec'argv' @:: Lens' ExecuteCommand (Data.Vector.Vector Data.ByteString.ByteString)@
         * 'Proto.Worker_Fields.env' @:: Lens' ExecuteCommand [ExecuteCommand'EnvironmentEntry]@
         * 'Proto.Worker_Fields.vec'env' @:: Lens' ExecuteCommand (Data.Vector.Vector ExecuteCommand'EnvironmentEntry)@ -}
data ExecuteCommand
  = ExecuteCommand'_constructor {_ExecuteCommand'argv :: !(Data.Vector.Vector Data.ByteString.ByteString),
                                 _ExecuteCommand'env :: !(Data.Vector.Vector ExecuteCommand'EnvironmentEntry),
                                 _ExecuteCommand'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExecuteCommand where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExecuteCommand "argv" [Data.ByteString.ByteString] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExecuteCommand'argv
           (\ x__ y__ -> x__ {_ExecuteCommand'argv = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExecuteCommand "vec'argv" (Data.Vector.Vector Data.ByteString.ByteString) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExecuteCommand'argv
           (\ x__ y__ -> x__ {_ExecuteCommand'argv = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExecuteCommand "env" [ExecuteCommand'EnvironmentEntry] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExecuteCommand'env (\ x__ y__ -> x__ {_ExecuteCommand'env = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExecuteCommand "vec'env" (Data.Vector.Vector ExecuteCommand'EnvironmentEntry) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExecuteCommand'env (\ x__ y__ -> x__ {_ExecuteCommand'env = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExecuteCommand where
  messageName _ = Data.Text.pack "worker.ExecuteCommand"
  packedMessageDescriptor _
    = "\n\
      \\SOExecuteCommand\DC2\DC2\n\
      \\EOTargv\CAN\SOH \ETX(\fR\EOTargv\DC29\n\
      \\ETXenv\CAN\STX \ETX(\v2'.worker.ExecuteCommand.EnvironmentEntryR\ETXenv\SUB:\n\
      \\DLEEnvironmentEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\fR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\fR\ENQvalue"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        argv__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "argv"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"argv")) ::
              Data.ProtoLens.FieldDescriptor ExecuteCommand
        env__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "env"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExecuteCommand'EnvironmentEntry)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"env")) ::
              Data.ProtoLens.FieldDescriptor ExecuteCommand
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, argv__field_descriptor),
           (Data.ProtoLens.Tag 2, env__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExecuteCommand'_unknownFields
        (\ x__ y__ -> x__ {_ExecuteCommand'_unknownFields = y__})
  defMessage
    = ExecuteCommand'_constructor
        {_ExecuteCommand'argv = Data.Vector.Generic.empty,
         _ExecuteCommand'env = Data.Vector.Generic.empty,
         _ExecuteCommand'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExecuteCommand
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.ByteString.ByteString
             -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld ExecuteCommand'EnvironmentEntry
                -> Data.ProtoLens.Encoding.Bytes.Parser ExecuteCommand
        loop x mutable'argv mutable'env
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'argv <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'argv)
                      frozen'env <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                      (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'env)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'argv") frozen'argv
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"vec'env") frozen'env x)))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.getBytes
                                              (Prelude.fromIntegral len))
                                        "argv"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'argv y)
                                loop x v mutable'env
                        18
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "env"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'env y)
                                loop x mutable'argv v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'argv mutable'env
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'argv <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                Data.ProtoLens.Encoding.Growing.new
              mutable'env <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                               Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'argv mutable'env)
          "ExecuteCommand"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           _v))
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'argv") _x))
             ((Data.Monoid.<>)
                (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                   (\ _v
                      -> (Data.Monoid.<>)
                           (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                           ((Prelude..)
                              (\ bs
                                 -> (Data.Monoid.<>)
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                         (Prelude.fromIntegral (Data.ByteString.length bs)))
                                      (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                              Data.ProtoLens.encodeMessage _v))
                   (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'env") _x))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExecuteCommand where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExecuteCommand'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExecuteCommand'argv x__)
                (Control.DeepSeq.deepseq (_ExecuteCommand'env x__) ()))
{- | Fields :
     
         * 'Proto.Worker_Fields.key' @:: Lens' ExecuteCommand'EnvironmentEntry Data.ByteString.ByteString@
         * 'Proto.Worker_Fields.value' @:: Lens' ExecuteCommand'EnvironmentEntry Data.ByteString.ByteString@ -}
data ExecuteCommand'EnvironmentEntry
  = ExecuteCommand'EnvironmentEntry'_constructor {_ExecuteCommand'EnvironmentEntry'key :: !Data.ByteString.ByteString,
                                                  _ExecuteCommand'EnvironmentEntry'value :: !Data.ByteString.ByteString,
                                                  _ExecuteCommand'EnvironmentEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExecuteCommand'EnvironmentEntry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExecuteCommand'EnvironmentEntry "key" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExecuteCommand'EnvironmentEntry'key
           (\ x__ y__ -> x__ {_ExecuteCommand'EnvironmentEntry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExecuteCommand'EnvironmentEntry "value" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExecuteCommand'EnvironmentEntry'value
           (\ x__ y__ -> x__ {_ExecuteCommand'EnvironmentEntry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExecuteCommand'EnvironmentEntry where
  messageName _
    = Data.Text.pack "worker.ExecuteCommand.EnvironmentEntry"
  packedMessageDescriptor _
    = "\n\
      \\DLEEnvironmentEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\fR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\fR\ENQvalue"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor ExecuteCommand'EnvironmentEntry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor ExecuteCommand'EnvironmentEntry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExecuteCommand'EnvironmentEntry'_unknownFields
        (\ x__ y__
           -> x__ {_ExecuteCommand'EnvironmentEntry'_unknownFields = y__})
  defMessage
    = ExecuteCommand'EnvironmentEntry'_constructor
        {_ExecuteCommand'EnvironmentEntry'key = Data.ProtoLens.fieldDefault,
         _ExecuteCommand'EnvironmentEntry'value = Data.ProtoLens.fieldDefault,
         _ExecuteCommand'EnvironmentEntry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExecuteCommand'EnvironmentEntry
          -> Data.ProtoLens.Encoding.Bytes.Parser ExecuteCommand'EnvironmentEntry
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "EnvironmentEntry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((\ bs
                          -> (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                  (Prelude.fromIntegral (Data.ByteString.length bs)))
                               (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"value") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExecuteCommand'EnvironmentEntry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExecuteCommand'EnvironmentEntry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExecuteCommand'EnvironmentEntry'key x__)
                (Control.DeepSeq.deepseq
                   (_ExecuteCommand'EnvironmentEntry'value x__) ()))
{- | Fields :
     
         * 'Proto.Worker_Fields.maybe'data'' @:: Lens' ExecuteEvent (Prelude.Maybe ExecuteEvent'Data)@
         * 'Proto.Worker_Fields.maybe'command' @:: Lens' ExecuteEvent (Prelude.Maybe ExecuteCommand)@
         * 'Proto.Worker_Fields.command' @:: Lens' ExecuteEvent ExecuteCommand@
         * 'Proto.Worker_Fields.maybe'cancel' @:: Lens' ExecuteEvent (Prelude.Maybe ExecuteCancel)@
         * 'Proto.Worker_Fields.cancel' @:: Lens' ExecuteEvent ExecuteCancel@ -}
data ExecuteEvent
  = ExecuteEvent'_constructor {_ExecuteEvent'data' :: !(Prelude.Maybe ExecuteEvent'Data),
                               _ExecuteEvent'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExecuteEvent where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data ExecuteEvent'Data
  = ExecuteEvent'Command !ExecuteCommand |
    ExecuteEvent'Cancel !ExecuteCancel
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField ExecuteEvent "maybe'data'" (Prelude.Maybe ExecuteEvent'Data) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExecuteEvent'data' (\ x__ y__ -> x__ {_ExecuteEvent'data' = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExecuteEvent "maybe'command" (Prelude.Maybe ExecuteCommand) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExecuteEvent'data' (\ x__ y__ -> x__ {_ExecuteEvent'data' = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ExecuteEvent'Command x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap ExecuteEvent'Command y__))
instance Data.ProtoLens.Field.HasField ExecuteEvent "command" ExecuteCommand where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExecuteEvent'data' (\ x__ y__ -> x__ {_ExecuteEvent'data' = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ExecuteEvent'Command x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap ExecuteEvent'Command y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField ExecuteEvent "maybe'cancel" (Prelude.Maybe ExecuteCancel) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExecuteEvent'data' (\ x__ y__ -> x__ {_ExecuteEvent'data' = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ExecuteEvent'Cancel x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap ExecuteEvent'Cancel y__))
instance Data.ProtoLens.Field.HasField ExecuteEvent "cancel" ExecuteCancel where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExecuteEvent'data' (\ x__ y__ -> x__ {_ExecuteEvent'data' = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ExecuteEvent'Cancel x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap ExecuteEvent'Cancel y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message ExecuteEvent where
  messageName _ = Data.Text.pack "worker.ExecuteEvent"
  packedMessageDescriptor _
    = "\n\
      \\fExecuteEvent\DC22\n\
      \\acommand\CAN\SOH \SOH(\v2\SYN.worker.ExecuteCommandH\NULR\acommand\DC2/\n\
      \\ACKcancel\CAN\STX \SOH(\v2\NAK.worker.ExecuteCancelH\NULR\ACKcancelB\ACK\n\
      \\EOTdata"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        command__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "command"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExecuteCommand)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'command")) ::
              Data.ProtoLens.FieldDescriptor ExecuteEvent
        cancel__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "cancel"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExecuteCancel)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'cancel")) ::
              Data.ProtoLens.FieldDescriptor ExecuteEvent
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, command__field_descriptor),
           (Data.ProtoLens.Tag 2, cancel__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExecuteEvent'_unknownFields
        (\ x__ y__ -> x__ {_ExecuteEvent'_unknownFields = y__})
  defMessage
    = ExecuteEvent'_constructor
        {_ExecuteEvent'data' = Prelude.Nothing,
         _ExecuteEvent'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExecuteEvent -> Data.ProtoLens.Encoding.Bytes.Parser ExecuteEvent
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "command"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"command") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "cancel"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"cancel") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ExecuteEvent"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'data'") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just (ExecuteEvent'Command v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage v)
                (Prelude.Just (ExecuteEvent'Cancel v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ExecuteEvent where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExecuteEvent'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ExecuteEvent'data' x__) ())
instance Control.DeepSeq.NFData ExecuteEvent'Data where
  rnf (ExecuteEvent'Command x__) = Control.DeepSeq.rnf x__
  rnf (ExecuteEvent'Cancel x__) = Control.DeepSeq.rnf x__
_ExecuteEvent'Command ::
  Data.ProtoLens.Prism.Prism' ExecuteEvent'Data ExecuteCommand
_ExecuteEvent'Command
  = Data.ProtoLens.Prism.prism'
      ExecuteEvent'Command
      (\ p__
         -> case p__ of
              (ExecuteEvent'Command p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ExecuteEvent'Cancel ::
  Data.ProtoLens.Prism.Prism' ExecuteEvent'Data ExecuteCancel
_ExecuteEvent'Cancel
  = Data.ProtoLens.Prism.prism'
      ExecuteEvent'Cancel
      (\ p__
         -> case p__ of
              (ExecuteEvent'Cancel p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
{- | Fields :
     
         * 'Proto.Worker_Fields.exitCode' @:: Lens' ExecuteResponse Data.Int.Int32@
         * 'Proto.Worker_Fields.stderr' @:: Lens' ExecuteResponse Data.Text.Text@ -}
data ExecuteResponse
  = ExecuteResponse'_constructor {_ExecuteResponse'exitCode :: !Data.Int.Int32,
                                  _ExecuteResponse'stderr :: !Data.Text.Text,
                                  _ExecuteResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExecuteResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExecuteResponse "exitCode" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExecuteResponse'exitCode
           (\ x__ y__ -> x__ {_ExecuteResponse'exitCode = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExecuteResponse "stderr" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExecuteResponse'stderr
           (\ x__ y__ -> x__ {_ExecuteResponse'stderr = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExecuteResponse where
  messageName _ = Data.Text.pack "worker.ExecuteResponse"
  packedMessageDescriptor _
    = "\n\
      \\SIExecuteResponse\DC2\ESC\n\
      \\texit_code\CAN\SOH \SOH(\ENQR\bexitCode\DC2\SYN\n\
      \\ACKstderr\CAN\STX \SOH(\tR\ACKstderr"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        exitCode__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "exit_code"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"exitCode")) ::
              Data.ProtoLens.FieldDescriptor ExecuteResponse
        stderr__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "stderr"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"stderr")) ::
              Data.ProtoLens.FieldDescriptor ExecuteResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, exitCode__field_descriptor),
           (Data.ProtoLens.Tag 2, stderr__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExecuteResponse'_unknownFields
        (\ x__ y__ -> x__ {_ExecuteResponse'_unknownFields = y__})
  defMessage
    = ExecuteResponse'_constructor
        {_ExecuteResponse'exitCode = Data.ProtoLens.fieldDefault,
         _ExecuteResponse'stderr = Data.ProtoLens.fieldDefault,
         _ExecuteResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExecuteResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser ExecuteResponse
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "exit_code"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"exitCode") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "stderr"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"stderr") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ExecuteResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"exitCode") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"stderr") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8 _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExecuteResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExecuteResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExecuteResponse'exitCode x__)
                (Control.DeepSeq.deepseq (_ExecuteResponse'stderr x__) ()))
data Worker = Worker {}
instance Data.ProtoLens.Service.Types.Service Worker where
  type ServiceName Worker = "Worker"
  type ServicePackage Worker = "worker"
  type ServiceMethods Worker = '["exec", "execute"]
  packedServiceDescriptor _
    = "\n\
      \\ACKWorker\DC2<\n\
      \\aExecute\DC2\SYN.worker.ExecuteCommand\SUB\ETB.worker.ExecuteResponse\"\NUL\DC29\n\
      \\EOTExec\DC2\DC4.worker.ExecuteEvent\SUB\ETB.worker.ExecuteResponse\"\NUL(\SOH"
instance Data.ProtoLens.Service.Types.HasMethodImpl Worker "execute" where
  type MethodName Worker "execute" = "Execute"
  type MethodInput Worker "execute" = ExecuteCommand
  type MethodOutput Worker "execute" = ExecuteResponse
  type MethodStreamingType Worker "execute" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Worker "exec" where
  type MethodName Worker "exec" = "Exec"
  type MethodInput Worker "exec" = ExecuteEvent
  type MethodOutput Worker "exec" = ExecuteResponse
  type MethodStreamingType Worker "exec" = 'Data.ProtoLens.Service.Types.ClientStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\fworker.proto\DC2\ACKworker\"\155\SOH\n\
    \\SOExecuteCommand\DC2\DC2\n\
    \\EOTargv\CAN\SOH \ETX(\fR\EOTargv\DC29\n\
    \\ETXenv\CAN\STX \ETX(\v2'.worker.ExecuteCommand.EnvironmentEntryR\ETXenv\SUB:\n\
    \\DLEEnvironmentEntry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\fR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\fR\ENQvalue\"F\n\
    \\SIExecuteResponse\DC2\ESC\n\
    \\texit_code\CAN\SOH \SOH(\ENQR\bexitCode\DC2\SYN\n\
    \\ACKstderr\CAN\STX \SOH(\tR\ACKstderr\"\SI\n\
    \\rExecuteCancel\"{\n\
    \\fExecuteEvent\DC22\n\
    \\acommand\CAN\SOH \SOH(\v2\SYN.worker.ExecuteCommandH\NULR\acommand\DC2/\n\
    \\ACKcancel\CAN\STX \SOH(\v2\NAK.worker.ExecuteCancelH\NULR\ACKcancelB\ACK\n\
    \\EOTdata2\129\SOH\n\
    \\ACKWorker\DC2<\n\
    \\aExecute\DC2\SYN.worker.ExecuteCommand\SUB\ETB.worker.ExecuteResponse\"\NUL\DC29\n\
    \\EOTExec\DC2\DC4.worker.ExecuteEvent\SUB\ETB.worker.ExecuteResponse\"\NUL(\SOHB/\n\
    \\RScom.facebook.buck.worker.modelB\vWorkerProtoP\SOHJ\206\t\n\
    \\ACK\DC2\EOT\t\NUL.\SOH\n\
    \\181\STX\n\
    \\SOH\f\DC2\ETX\t\NUL\DC22\170\STX\n\
    \ Copyright (c) Meta Platforms, Inc. and affiliates.\n\
    \\n\
    \ This source code is licensed under both the MIT license found in the\n\
    \ LICENSE-MIT file in the root directory of this source tree and the Apache\n\
    \ License, Version 2.0 found in the LICENSE-APACHE file in the root directory\n\
    \ of this source tree.\n\
    \\n\
    \\b\n\
    \\SOH\b\DC2\ETX\v\NUL\"\n\
    \\t\n\
    \\STX\b\n\
    \\DC2\ETX\v\NUL\"\n\
    \\b\n\
    \\SOH\b\DC2\ETX\f\NUL7\n\
    \\t\n\
    \\STX\b\SOH\DC2\ETX\f\NUL7\n\
    \\b\n\
    \\SOH\b\DC2\ETX\r\NUL,\n\
    \\t\n\
    \\STX\b\b\DC2\ETX\r\NUL,\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\SI\NUL\SI\n\
    \\n\
    \\n\
    \\STX\EOT\NUL\DC2\EOT\DC1\NUL\EM\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\DC1\b\SYN\n\
    \\f\n\
    \\EOT\EOT\NUL\ETX\NUL\DC2\EOT\DC2\STX\NAK\ETX\n\
    \\f\n\
    \\ENQ\EOT\NUL\ETX\NUL\SOH\DC2\ETX\DC2\n\
    \\SUB\n\
    \\r\n\
    \\ACK\EOT\NUL\ETX\NUL\STX\NUL\DC2\ETX\DC3\EOT\DC2\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\NUL\ENQ\DC2\ETX\DC3\EOT\t\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\NUL\SOH\DC2\ETX\DC3\n\
    \\r\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\NUL\ETX\DC2\ETX\DC3\DLE\DC1\n\
    \\r\n\
    \\ACK\EOT\NUL\ETX\NUL\STX\SOH\DC2\ETX\DC4\EOT\DC4\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\SOH\ENQ\DC2\ETX\DC4\EOT\t\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\SOH\SOH\DC2\ETX\DC4\n\
    \\SI\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\SOH\ETX\DC2\ETX\DC4\DC2\DC3\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\ETB\STX\SUB\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\EOT\DC2\ETX\ETB\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ENQ\DC2\ETX\ETB\v\DLE\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\ETB\DC1\NAK\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\ETB\CAN\EM\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\SOH\DC2\ETX\CAN\STX$\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\EOT\DC2\ETX\CAN\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ACK\DC2\ETX\CAN\v\ESC\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\SOH\DC2\ETX\CAN\FS\US\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ETX\DC2\ETX\CAN\"#\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT\ESC\NUL\RS\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\ESC\b\ETB\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX\FS\STX\SYN\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ENQ\DC2\ETX\FS\STX\a\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX\FS\b\DC1\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX\FS\DC4\NAK\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\SOH\DC2\ETX\GS\STX\DC4\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ENQ\DC2\ETX\GS\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\SOH\DC2\ETX\GS\t\SI\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ETX\DC2\ETX\GS\DC2\DC3\n\
    \\t\n\
    \\STX\EOT\STX\DC2\ETX \NUL\CAN\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX \b\NAK\n\
    \\n\
    \\n\
    \\STX\EOT\ETX\DC2\EOT\"\NUL'\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ETX\SOH\DC2\ETX\"\b\DC4\n\
    \\f\n\
    \\EOT\EOT\ETX\b\NUL\DC2\EOT#\STX&\ETX\n\
    \\f\n\
    \\ENQ\EOT\ETX\b\NUL\SOH\DC2\ETX#\b\f\n\
    \\v\n\
    \\EOT\EOT\ETX\STX\NUL\DC2\ETX$\EOT\US\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ACK\DC2\ETX$\EOT\DC2\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\SOH\DC2\ETX$\DC3\SUB\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ETX\DC2\ETX$\GS\RS\n\
    \\v\n\
    \\EOT\EOT\ETX\STX\SOH\DC2\ETX%\EOT\GS\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\ACK\DC2\ETX%\EOT\DC1\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\SOH\DC2\ETX%\DC2\CAN\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\ETX\DC2\ETX%\ESC\FS\n\
    \\n\
    \\n\
    \\STX\ACK\NUL\DC2\EOT)\NUL.\SOH\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETX)\b\SO\n\
    \A\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\ETX+\STX:\SUB4 TODO(ctolliday) delete once workers switch to Exec\n\
    \\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETX+\ACK\r\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETX+\SO\FS\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETX+'6\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\SOH\DC2\ETX-\STX<\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\SOH\DC2\ETX-\ACK\n\
    \\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\ENQ\DC2\ETX-\v\DC1\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\STX\DC2\ETX-\DC2\RS\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\ETX\DC2\ETX-)8b\ACKproto3"
