module Types.ByteString where

import Data.ByteString (ByteString, fromStrict, toStrict)
import Data.ByteString.Lazy qualified as Lazy
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as Lazy.Text
import qualified Data.Text.Lazy.Encoding as Lazy

class Utf8 a where
  toUtf8 :: a -> ByteString
  toUtf8Lazy :: a -> Lazy.ByteString
  fromUtf8 :: ByteString -> a
  fromUtf8Lazy :: Lazy.ByteString -> a

instance Utf8 Text where
  toUtf8 = encodeUtf8
  toUtf8Lazy = fromStrict . toUtf8
  fromUtf8 = decodeUtf8
  fromUtf8Lazy = fromUtf8 . toStrict

instance Utf8 String where
  toUtf8 = toUtf8 . Text.pack
  toUtf8Lazy = Lazy.encodeUtf8 . Lazy.Text.pack
  fromUtf8 = Text.unpack . fromUtf8
  fromUtf8Lazy = Lazy.Text.unpack . Lazy.decodeUtf8
