{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.Text where

import Control.Exception (evaluate, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text as ST
import qualified Data.Text.Array as A
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as E
import qualified Data.Text.Internal as ST
import qualified Data.Text.Internal.Lazy as LT
import qualified Data.Text.Unsafe as U
import Data.Validity
import Data.Word

-- | A text is valid if the internal structure is consistent.
instance Validity ST.Text where
  validate t@(ST.Text arr off len) =
    mconcat
      [ declare "The length is positive." (len >= 0),
        declare "The offset is positive." (off >= 0),
        declare "The offset char is valid" $
          let c = A.unsafeIndex arr off
           in len == 0 || offsetCharCheck c,
        declare "The array contains bytes in the right encoding"
          $ (== (Right t :: Either E.UnicodeException ST.Text))
            . U.unsafeDupablePerformIO
            . try
            . evaluate
            . validityDecoding
            . LB.toStrict
            . SBB.toLazyByteString
            . mconcat
            . map validityWording
          $ A.toList arr off len
      ]
    where

#if MIN_VERSION_text(2,0,0)
      offsetCharCheck :: Word8 -> Bool
      offsetCharCheck c =  c < 0x80 || c >= 0xC0 -- Valid UTF8
#else
      offsetCharCheck :: Word16 -> Bool
      offsetCharCheck c = c < 0xDC00 || c > 0xDFFF -- Valid UTF16
#endif

#if MIN_VERSION_text(2,0,0)
      validityDecoding :: ByteString -> Text
      validityDecoding = E.decodeUtf8With E.strictDecode
#else
      validityDecoding :: ByteString -> Text
      validityDecoding = E.decodeUtf16LEWith E.strictDecode
#endif

#if MIN_VERSION_text(2,0,0)
      validityWording :: Word8 -> SBB.Builder
      validityWording = SBB.word8
#else
      validityWording :: Word16 -> SBB.Builder
      validityWording = SBB.word16LE
#endif

-- | A lazy text value is valid if all the internal chunks are valid and nonempty
instance Validity LT.Text where
  validate LT.Empty = valid
  validate (LT.Chunk st lt) =
    mconcat
      [ delve "The strict chunk" st,
        declare "The strict chunk is not empty" $ not $ ST.null st,
        delve "The lazy chunk" lt
      ]

validateTextSingleLine :: ST.Text -> Validation
validateTextSingleLine = validateStringSingleLine . ST.unpack

decorateText :: ST.Text -> (Char -> Validation) -> Validation
decorateText t func = mconcat $
  flip map (zip [0 ..] (ST.unpack t)) $ \(i, c) ->
    decorate (unwords ["The character at index", show (i :: Integer), "in the text"]) $
      func c
