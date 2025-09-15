{- This file was auto-generated from instrument.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Instrument (
        Instrument(..), Empty(), Event(), Options(), RebuildRequest()
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
      -}
data Empty
  = Empty'_constructor {_Empty'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Empty where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message Empty where
  messageName _ = Data.Text.pack "instrument.Empty"
  packedMessageDescriptor _
    = "\n\
      \\ENQEmpty"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Empty'_unknownFields
        (\ x__ y__ -> x__ {_Empty'_unknownFields = y__})
  defMessage = Empty'_constructor {_Empty'_unknownFields = []}
  parseMessage
    = let
        loop :: Empty -> Data.ProtoLens.Encoding.Bytes.Parser Empty
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
          (do loop Data.ProtoLens.defMessage) "Empty"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData Empty where
  rnf
    = \ x__ -> Control.DeepSeq.deepseq (_Empty'_unknownFields x__) ()
{- | Fields :
     
         * 'Proto.Instrument_Fields.encoded' @:: Lens' Event Data.ByteString.ByteString@ -}
data Event
  = Event'_constructor {_Event'encoded :: !Data.ByteString.ByteString,
                        _Event'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Event where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Event "encoded" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Event'encoded (\ x__ y__ -> x__ {_Event'encoded = y__}))
        Prelude.id
instance Data.ProtoLens.Message Event where
  messageName _ = Data.Text.pack "instrument.Event"
  packedMessageDescriptor _
    = "\n\
      \\ENQEvent\DC2\CAN\n\
      \\aencoded\CAN\SOH \SOH(\fR\aencoded"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        encoded__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "encoded"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"encoded")) ::
              Data.ProtoLens.FieldDescriptor Event
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, encoded__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Event'_unknownFields
        (\ x__ y__ -> x__ {_Event'_unknownFields = y__})
  defMessage
    = Event'_constructor
        {_Event'encoded = Data.ProtoLens.fieldDefault,
         _Event'_unknownFields = []}
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
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "encoded"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"encoded") y x)
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
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"encoded") _x
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
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData Event where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Event'_unknownFields x__)
             (Control.DeepSeq.deepseq (_Event'encoded x__) ())
{- | Fields :
     
         * 'Proto.Instrument_Fields.extraGhcOptions' @:: Lens' Options Data.Text.Text@ -}
data Options
  = Options'_constructor {_Options'extraGhcOptions :: !Data.Text.Text,
                          _Options'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Options where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Options "extraGhcOptions" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Options'extraGhcOptions
           (\ x__ y__ -> x__ {_Options'extraGhcOptions = y__}))
        Prelude.id
instance Data.ProtoLens.Message Options where
  messageName _ = Data.Text.pack "instrument.Options"
  packedMessageDescriptor _
    = "\n\
      \\aOptions\DC2*\n\
      \\DC1extra_ghc_options\CAN\SOH \SOH(\tR\SIextraGhcOptions"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        extraGhcOptions__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "extra_ghc_options"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"extraGhcOptions")) ::
              Data.ProtoLens.FieldDescriptor Options
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, extraGhcOptions__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Options'_unknownFields
        (\ x__ y__ -> x__ {_Options'_unknownFields = y__})
  defMessage
    = Options'_constructor
        {_Options'extraGhcOptions = Data.ProtoLens.fieldDefault,
         _Options'_unknownFields = []}
  parseMessage
    = let
        loop :: Options -> Data.ProtoLens.Encoding.Bytes.Parser Options
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
                                       "extra_ghc_options"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"extraGhcOptions") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Options"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view
                      (Data.ProtoLens.Field.field @"extraGhcOptions") _x
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
instance Control.DeepSeq.NFData Options where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Options'_unknownFields x__)
             (Control.DeepSeq.deepseq (_Options'extraGhcOptions x__) ())
{- | Fields :
     
         * 'Proto.Instrument_Fields.target' @:: Lens' RebuildRequest Data.Text.Text@ -}
data RebuildRequest
  = RebuildRequest'_constructor {_RebuildRequest'target :: !Data.Text.Text,
                                 _RebuildRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show RebuildRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField RebuildRequest "target" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _RebuildRequest'target
           (\ x__ y__ -> x__ {_RebuildRequest'target = y__}))
        Prelude.id
instance Data.ProtoLens.Message RebuildRequest where
  messageName _ = Data.Text.pack "instrument.RebuildRequest"
  packedMessageDescriptor _
    = "\n\
      \\SORebuildRequest\DC2\SYN\n\
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
              Data.ProtoLens.FieldDescriptor RebuildRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, target__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _RebuildRequest'_unknownFields
        (\ x__ y__ -> x__ {_RebuildRequest'_unknownFields = y__})
  defMessage
    = RebuildRequest'_constructor
        {_RebuildRequest'target = Data.ProtoLens.fieldDefault,
         _RebuildRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          RebuildRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser RebuildRequest
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
          (do loop Data.ProtoLens.defMessage) "RebuildRequest"
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
instance Control.DeepSeq.NFData RebuildRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_RebuildRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq (_RebuildRequest'target x__) ())
data Instrument = Instrument {}
instance Data.ProtoLens.Service.Types.Service Instrument where
  type ServiceName Instrument = "Instrument"
  type ServicePackage Instrument = "instrument"
  type ServiceMethods Instrument = '["notifyMe",
                                     "setOptions",
                                     "triggerRebuild"]
  packedServiceDescriptor _
    = "\n\
      \\n\
      \Instrument\DC24\n\
      \\bNotifyMe\DC2\DC1.instrument.Empty\SUB\DC1.instrument.Event\"\NUL0\SOH\DC26\n\
      \\n\
      \SetOptions\DC2\DC3.instrument.Options\SUB\DC1.instrument.Empty\"\NUL\DC2A\n\
      \\SOTriggerRebuild\DC2\SUB.instrument.RebuildRequest\SUB\DC1.instrument.Empty\"\NUL"
instance Data.ProtoLens.Service.Types.HasMethodImpl Instrument "notifyMe" where
  type MethodName Instrument "notifyMe" = "NotifyMe"
  type MethodInput Instrument "notifyMe" = Empty
  type MethodOutput Instrument "notifyMe" = Event
  type MethodStreamingType Instrument "notifyMe" = 'Data.ProtoLens.Service.Types.ServerStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Instrument "setOptions" where
  type MethodName Instrument "setOptions" = "SetOptions"
  type MethodInput Instrument "setOptions" = Options
  type MethodOutput Instrument "setOptions" = Empty
  type MethodStreamingType Instrument "setOptions" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Instrument "triggerRebuild" where
  type MethodName Instrument "triggerRebuild" = "TriggerRebuild"
  type MethodInput Instrument "triggerRebuild" = RebuildRequest
  type MethodOutput Instrument "triggerRebuild" = Empty
  type MethodStreamingType Instrument "triggerRebuild" = 'Data.ProtoLens.Service.Types.NonStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\DLEinstrument.proto\DC2\n\
    \instrument\"\a\n\
    \\ENQEmpty\"!\n\
    \\ENQEvent\DC2\CAN\n\
    \\aencoded\CAN\SOH \SOH(\fR\aencoded\"5\n\
    \\aOptions\DC2*\n\
    \\DC1extra_ghc_options\CAN\SOH \SOH(\tR\SIextraGhcOptions\"(\n\
    \\SORebuildRequest\DC2\SYN\n\
    \\ACKtarget\CAN\SOH \SOH(\tR\ACKtarget2\189\SOH\n\
    \\n\
    \Instrument\DC24\n\
    \\bNotifyMe\DC2\DC1.instrument.Empty\SUB\DC1.instrument.Event\"\NUL0\SOH\DC26\n\
    \\n\
    \SetOptions\DC2\DC3.instrument.Options\SUB\DC1.instrument.Empty\"\NUL\DC2A\n\
    \\SOTriggerRebuild\DC2\SUB.instrument.RebuildRequest\SUB\DC1.instrument.Empty\"\NULJ\235\ETX\n\
    \\ACK\DC2\EOT\NUL\NUL\SYN\SOH\n\
    \\b\n\
    \\SOH\f\DC2\ETX\NUL\NUL\DC2\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\STX\NUL\DC3\n\
    \\t\n\
    \\STX\EOT\NUL\DC2\ETX\EOT\NUL\DLE\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\EOT\b\r\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT\ACK\NUL\b\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\ACK\b\r\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX\a\STX\DC4\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ENQ\DC2\ETX\a\STX\a\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX\a\b\SI\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX\a\DC2\DC3\n\
    \\n\
    \\n\
    \\STX\EOT\STX\DC2\EOT\n\
    \\NUL\f\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX\n\
    \\b\SI\n\
    \\v\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETX\v\STX\US\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ENQ\DC2\ETX\v\STX\b\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETX\v\t\SUB\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETX\v\GS\RS\n\
    \\n\
    \\n\
    \\STX\EOT\ETX\DC2\EOT\SO\NUL\DLE\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ETX\SOH\DC2\ETX\SO\b\SYN\n\
    \\v\n\
    \\EOT\EOT\ETX\STX\NUL\DC2\ETX\SI\STX\DC4\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ENQ\DC2\ETX\SI\STX\b\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\SOH\DC2\ETX\SI\t\SI\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ETX\DC2\ETX\SI\DC2\DC3\n\
    \\n\
    \\n\
    \\STX\ACK\NUL\DC2\EOT\DC2\NUL\SYN\SOH\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETX\DC2\b\DC2\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\ETX\DC3\STX/\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETX\DC3\ACK\SO\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETX\DC3\SI\DC4\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ACK\DC2\ETX\DC3\US%\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETX\DC3&+\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\SOH\DC2\ETX\DC4\STX,\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\SOH\DC2\ETX\DC4\ACK\DLE\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\STX\DC2\ETX\DC4\DC1\CAN\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\ETX\DC2\ETX\DC4#(\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\STX\DC2\ETX\NAK\STX7\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\SOH\DC2\ETX\NAK\ACK\DC4\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\STX\DC2\ETX\NAK\NAK#\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\ETX\DC2\ETX\NAK.3b\ACKproto3"