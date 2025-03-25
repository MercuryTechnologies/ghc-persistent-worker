{- This file was auto-generated from instrument.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Instrument (
        Instrument(..), CompileEnd(), CompileStart(), Event(),
        Event'Event(..), _Event'CompileStart, _Event'CompileEnd,
        _Event'Halt, Halt(), Unit()
    ) where
import qualified Control.DeepSeq
import qualified Data.ProtoLens.Prism
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
{- | Fields :
     
         * 'Proto.Instrument_Fields.target' @:: Lens' CompileEnd Data.Text.Text@
         * 'Proto.Instrument_Fields.exitCode' @:: Lens' CompileEnd Data.Int.Int32@
         * 'Proto.Instrument_Fields.stderr' @:: Lens' CompileEnd Data.Text.Text@ -}
data CompileEnd
  = CompileEnd'_constructor {_CompileEnd'target :: !Data.Text.Text,
                             _CompileEnd'exitCode :: !Data.Int.Int32,
                             _CompileEnd'stderr :: !Data.Text.Text,
                             _CompileEnd'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show CompileEnd where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField CompileEnd "target" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CompileEnd'target (\ x__ y__ -> x__ {_CompileEnd'target = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField CompileEnd "exitCode" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CompileEnd'exitCode
           (\ x__ y__ -> x__ {_CompileEnd'exitCode = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField CompileEnd "stderr" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CompileEnd'stderr (\ x__ y__ -> x__ {_CompileEnd'stderr = y__}))
        Prelude.id
instance Data.ProtoLens.Message CompileEnd where
  messageName _ = Data.Text.pack "instrument.CompileEnd"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \CompileEnd\DC2\SYN\n\
      \\ACKtarget\CAN\SOH \SOH(\tR\ACKtarget\DC2\ESC\n\
      \\texit_code\CAN\STX \SOH(\ENQR\bexitCode\DC2\SYN\n\
      \\ACKstderr\CAN\ETX \SOH(\tR\ACKstderr"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        target__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "target"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"target")) ::
              Data.ProtoLens.FieldDescriptor CompileEnd
        exitCode__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "exit_code"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"exitCode")) ::
              Data.ProtoLens.FieldDescriptor CompileEnd
        stderr__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "stderr"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"stderr")) ::
              Data.ProtoLens.FieldDescriptor CompileEnd
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, target__field_descriptor),
           (Data.ProtoLens.Tag 2, exitCode__field_descriptor),
           (Data.ProtoLens.Tag 3, stderr__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _CompileEnd'_unknownFields
        (\ x__ y__ -> x__ {_CompileEnd'_unknownFields = y__})
  defMessage
    = CompileEnd'_constructor
        {_CompileEnd'target = Data.ProtoLens.fieldDefault,
         _CompileEnd'exitCode = Data.ProtoLens.fieldDefault,
         _CompileEnd'stderr = Data.ProtoLens.fieldDefault,
         _CompileEnd'_unknownFields = []}
  parseMessage
    = let
        loop ::
          CompileEnd -> Data.ProtoLens.Encoding.Bytes.Parser CompileEnd
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
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "target"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"target") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "exit_code"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"exitCode") y x)
                        26
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
          (do loop Data.ProtoLens.defMessage) "CompileEnd"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"target") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"exitCode") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
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
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                            ((Prelude..)
                               (\ bs
                                  -> (Data.Monoid.<>)
                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                          (Prelude.fromIntegral (Data.ByteString.length bs)))
                                       (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                               Data.Text.Encoding.encodeUtf8 _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData CompileEnd where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_CompileEnd'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_CompileEnd'target x__)
                (Control.DeepSeq.deepseq
                   (_CompileEnd'exitCode x__)
                   (Control.DeepSeq.deepseq (_CompileEnd'stderr x__) ())))
{- | Fields :
     
         * 'Proto.Instrument_Fields.target' @:: Lens' CompileStart Data.Text.Text@ -}
data CompileStart
  = CompileStart'_constructor {_CompileStart'target :: !Data.Text.Text,
                               _CompileStart'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show CompileStart where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField CompileStart "target" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CompileStart'target
           (\ x__ y__ -> x__ {_CompileStart'target = y__}))
        Prelude.id
instance Data.ProtoLens.Message CompileStart where
  messageName _ = Data.Text.pack "instrument.CompileStart"
  packedMessageDescriptor _
    = "\n\
      \\fCompileStart\DC2\SYN\n\
      \\ACKtarget\CAN\SOH \SOH(\tR\ACKtarget"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        target__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "target"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"target")) ::
              Data.ProtoLens.FieldDescriptor CompileStart
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, target__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _CompileStart'_unknownFields
        (\ x__ y__ -> x__ {_CompileStart'_unknownFields = y__})
  defMessage
    = CompileStart'_constructor
        {_CompileStart'target = Data.ProtoLens.fieldDefault,
         _CompileStart'_unknownFields = []}
  parseMessage
    = let
        loop ::
          CompileStart -> Data.ProtoLens.Encoding.Bytes.Parser CompileStart
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
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "target"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"target") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "CompileStart"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"target") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData CompileStart where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_CompileStart'_unknownFields x__)
             (Control.DeepSeq.deepseq (_CompileStart'target x__) ())
{- | Fields :
     
         * 'Proto.Instrument_Fields.maybe'event' @:: Lens' Event (Prelude.Maybe Event'Event)@
         * 'Proto.Instrument_Fields.maybe'compileStart' @:: Lens' Event (Prelude.Maybe CompileStart)@
         * 'Proto.Instrument_Fields.compileStart' @:: Lens' Event CompileStart@
         * 'Proto.Instrument_Fields.maybe'compileEnd' @:: Lens' Event (Prelude.Maybe CompileEnd)@
         * 'Proto.Instrument_Fields.compileEnd' @:: Lens' Event CompileEnd@
         * 'Proto.Instrument_Fields.maybe'halt' @:: Lens' Event (Prelude.Maybe Halt)@
         * 'Proto.Instrument_Fields.halt' @:: Lens' Event Halt@ -}
data Event
  = Event'_constructor {_Event'event :: !(Prelude.Maybe Event'Event),
                        _Event'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Event where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data Event'Event
  = Event'CompileStart !CompileStart |
    Event'CompileEnd !CompileEnd |
    Event'Halt !Halt
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField Event "maybe'event" (Prelude.Maybe Event'Event) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Event'event (\ x__ y__ -> x__ {_Event'event = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Event "maybe'compileStart" (Prelude.Maybe CompileStart) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Event'event (\ x__ y__ -> x__ {_Event'event = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (Event'CompileStart x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap Event'CompileStart y__))
instance Data.ProtoLens.Field.HasField Event "compileStart" CompileStart where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Event'event (\ x__ y__ -> x__ {_Event'event = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (Event'CompileStart x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap Event'CompileStart y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField Event "maybe'compileEnd" (Prelude.Maybe CompileEnd) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Event'event (\ x__ y__ -> x__ {_Event'event = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (Event'CompileEnd x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap Event'CompileEnd y__))
instance Data.ProtoLens.Field.HasField Event "compileEnd" CompileEnd where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Event'event (\ x__ y__ -> x__ {_Event'event = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (Event'CompileEnd x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap Event'CompileEnd y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField Event "maybe'halt" (Prelude.Maybe Halt) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Event'event (\ x__ y__ -> x__ {_Event'event = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (Event'Halt x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap Event'Halt y__))
instance Data.ProtoLens.Field.HasField Event "halt" Halt where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Event'event (\ x__ y__ -> x__ {_Event'event = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (Event'Halt x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap Event'Halt y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message Event where
  messageName _ = Data.Text.pack "instrument.Event"
  packedMessageDescriptor _
    = "\n\
      \\ENQEvent\DC2>\n\
      \\fcompileStart\CAN\SOH \SOH(\v2\CAN.instrument.CompileStartH\NULR\fcompileStart\DC28\n\
      \\n\
      \compileEnd\CAN\STX \SOH(\v2\SYN.instrument.CompileEndH\NULR\n\
      \compileEnd\DC2&\n\
      \\EOThalt\CAN\ETX \SOH(\v2\DLE.instrument.HaltH\NULR\EOThaltB\a\n\
      \\ENQevent"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        compileStart__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "compileStart"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor CompileStart)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'compileStart")) ::
              Data.ProtoLens.FieldDescriptor Event
        compileEnd__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "compileEnd"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor CompileEnd)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'compileEnd")) ::
              Data.ProtoLens.FieldDescriptor Event
        halt__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "halt"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Halt)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'halt")) ::
              Data.ProtoLens.FieldDescriptor Event
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, compileStart__field_descriptor),
           (Data.ProtoLens.Tag 2, compileEnd__field_descriptor),
           (Data.ProtoLens.Tag 3, halt__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Event'_unknownFields
        (\ x__ y__ -> x__ {_Event'_unknownFields = y__})
  defMessage
    = Event'_constructor
        {_Event'event = Prelude.Nothing, _Event'_unknownFields = []}
  parseMessage
    = let
        loop :: Event -> Data.ProtoLens.Encoding.Bytes.Parser Event
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
                                       "compileStart"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"compileStart") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "compileEnd"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"compileEnd") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "halt"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"halt") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Event"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'event") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just (Event'CompileStart v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage v)
                (Prelude.Just (Event'CompileEnd v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage v)
                (Prelude.Just (Event'Halt v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData Event where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Event'_unknownFields x__)
             (Control.DeepSeq.deepseq (_Event'event x__) ())
instance Control.DeepSeq.NFData Event'Event where
  rnf (Event'CompileStart x__) = Control.DeepSeq.rnf x__
  rnf (Event'CompileEnd x__) = Control.DeepSeq.rnf x__
  rnf (Event'Halt x__) = Control.DeepSeq.rnf x__
_Event'CompileStart ::
  Data.ProtoLens.Prism.Prism' Event'Event CompileStart
_Event'CompileStart
  = Data.ProtoLens.Prism.prism'
      Event'CompileStart
      (\ p__
         -> case p__ of
              (Event'CompileStart p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_Event'CompileEnd ::
  Data.ProtoLens.Prism.Prism' Event'Event CompileEnd
_Event'CompileEnd
  = Data.ProtoLens.Prism.prism'
      Event'CompileEnd
      (\ p__
         -> case p__ of
              (Event'CompileEnd p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_Event'Halt :: Data.ProtoLens.Prism.Prism' Event'Event Halt
_Event'Halt
  = Data.ProtoLens.Prism.prism'
      Event'Halt
      (\ p__
         -> case p__ of
              (Event'Halt p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
{- | Fields :
      -}
data Halt
  = Halt'_constructor {_Halt'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Halt where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message Halt where
  messageName _ = Data.Text.pack "instrument.Halt"
  packedMessageDescriptor _
    = "\n\
      \\EOTHalt"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Halt'_unknownFields
        (\ x__ y__ -> x__ {_Halt'_unknownFields = y__})
  defMessage = Halt'_constructor {_Halt'_unknownFields = []}
  parseMessage
    = let
        loop :: Halt -> Data.ProtoLens.Encoding.Bytes.Parser Halt
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
          (do loop Data.ProtoLens.defMessage) "Halt"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData Halt where
  rnf
    = \ x__ -> Control.DeepSeq.deepseq (_Halt'_unknownFields x__) ()
{- | Fields :
      -}
data Unit
  = Unit'_constructor {_Unit'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Unit where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message Unit where
  messageName _ = Data.Text.pack "instrument.Unit"
  packedMessageDescriptor _
    = "\n\
      \\EOTUnit"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Unit'_unknownFields
        (\ x__ y__ -> x__ {_Unit'_unknownFields = y__})
  defMessage = Unit'_constructor {_Unit'_unknownFields = []}
  parseMessage
    = let
        loop :: Unit -> Data.ProtoLens.Encoding.Bytes.Parser Unit
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
          (do loop Data.ProtoLens.defMessage) "Unit"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData Unit where
  rnf
    = \ x__ -> Control.DeepSeq.deepseq (_Unit'_unknownFields x__) ()
data Instrument = Instrument {}
instance Data.ProtoLens.Service.Types.Service Instrument where
  type ServiceName Instrument = "Instrument"
  type ServicePackage Instrument = "instrument"
  type ServiceMethods Instrument = '["notifyMe"]
  packedServiceDescriptor _
    = "\n\
      \\n\
      \Instrument\DC23\n\
      \\bNotifyMe\DC2\DLE.instrument.Unit\SUB\DC1.instrument.Event\"\NUL0\SOH"
instance Data.ProtoLens.Service.Types.HasMethodImpl Instrument "notifyMe" where
  type MethodName Instrument "notifyMe" = "NotifyMe"
  type MethodInput Instrument "notifyMe" = Unit
  type MethodOutput Instrument "notifyMe" = Event
  type MethodStreamingType Instrument "notifyMe" = 'Data.ProtoLens.Service.Types.ServerStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\DLEinstrument.proto\DC2\n\
    \instrument\"\ACK\n\
    \\EOTUnit\"&\n\
    \\fCompileStart\DC2\SYN\n\
    \\ACKtarget\CAN\SOH \SOH(\tR\ACKtarget\"Y\n\
    \\n\
    \CompileEnd\DC2\SYN\n\
    \\ACKtarget\CAN\SOH \SOH(\tR\ACKtarget\DC2\ESC\n\
    \\texit_code\CAN\STX \SOH(\ENQR\bexitCode\DC2\SYN\n\
    \\ACKstderr\CAN\ETX \SOH(\tR\ACKstderr\"\ACK\n\
    \\EOTHalt\"\178\SOH\n\
    \\ENQEvent\DC2>\n\
    \\fcompileStart\CAN\SOH \SOH(\v2\CAN.instrument.CompileStartH\NULR\fcompileStart\DC28\n\
    \\n\
    \compileEnd\CAN\STX \SOH(\v2\SYN.instrument.CompileEndH\NULR\n\
    \compileEnd\DC2&\n\
    \\EOThalt\CAN\ETX \SOH(\v2\DLE.instrument.HaltH\NULR\EOThaltB\a\n\
    \\ENQevent2A\n\
    \\n\
    \Instrument\DC23\n\
    \\bNotifyMe\DC2\DLE.instrument.Unit\SUB\DC1.instrument.Event\"\NUL0\SOHJ\140\ENQ\n\
    \\ACK\DC2\EOT\NUL\NUL\FS\SOH\n\
    \\b\n\
    \\SOH\f\DC2\ETX\NUL\NUL\DC2\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\STX\NUL\DC3\n\
    \\t\n\
    \\STX\EOT\NUL\DC2\ETX\EOT\NUL\SI\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\EOT\b\f\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT\ACK\NUL\b\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\ACK\b\DC4\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX\a\STX\DC4\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ENQ\DC2\ETX\a\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX\a\t\SI\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX\a\DC2\DC3\n\
    \\n\
    \\n\
    \\STX\EOT\STX\DC2\EOT\n\
    \\NUL\SO\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX\n\
    \\b\DC2\n\
    \\v\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETX\v\STX\DC4\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ENQ\DC2\ETX\v\STX\b\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETX\v\t\SI\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETX\v\DC2\DC3\n\
    \\v\n\
    \\EOT\EOT\STX\STX\SOH\DC2\ETX\f\STX\SYN\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ENQ\DC2\ETX\f\STX\a\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\SOH\DC2\ETX\f\b\DC1\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ETX\DC2\ETX\f\DC4\NAK\n\
    \\v\n\
    \\EOT\EOT\STX\STX\STX\DC2\ETX\r\STX\DC4\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\ENQ\DC2\ETX\r\STX\b\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\SOH\DC2\ETX\r\t\SI\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\ETX\DC2\ETX\r\DC2\DC3\n\
    \\t\n\
    \\STX\EOT\ETX\DC2\ETX\DLE\NUL\SI\n\
    \\n\
    \\n\
    \\ETX\EOT\ETX\SOH\DC2\ETX\DLE\b\f\n\
    \\n\
    \\n\
    \\STX\EOT\EOT\DC2\EOT\DC2\NUL\CAN\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\EOT\SOH\DC2\ETX\DC2\b\r\n\
    \\f\n\
    \\EOT\EOT\EOT\b\NUL\DC2\EOT\DC3\STX\ETB\ETX\n\
    \\f\n\
    \\ENQ\EOT\EOT\b\NUL\SOH\DC2\ETX\DC3\b\r\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\NUL\DC2\ETX\DC4\EOT\"\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ACK\DC2\ETX\DC4\EOT\DLE\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\SOH\DC2\ETX\DC4\DC1\GS\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ETX\DC2\ETX\DC4 !\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\SOH\DC2\ETX\NAK\EOT\RS\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\ACK\DC2\ETX\NAK\EOT\SO\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\SOH\DC2\ETX\NAK\SI\EM\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\ETX\DC2\ETX\NAK\FS\GS\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\STX\DC2\ETX\SYN\EOT\DC2\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\STX\ACK\DC2\ETX\SYN\EOT\b\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\STX\SOH\DC2\ETX\SYN\t\r\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\STX\ETX\DC2\ETX\SYN\DLE\DC1\n\
    \\n\
    \\n\
    \\STX\ACK\NUL\DC2\EOT\SUB\NUL\FS\SOH\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETX\SUB\b\DC2\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\ETX\ESC\STX.\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETX\ESC\ACK\SO\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETX\ESC\SI\DC3\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ACK\DC2\ETX\ESC\RS$\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETX\ESC%*b\ACKproto3"