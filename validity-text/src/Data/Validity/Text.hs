{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.Text where

import Control.Exception (evaluate, try)
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as ST
import qualified Data.Text.Array as A
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as E
import qualified Data.Text.Internal as ST
import qualified Data.Text.Internal.Lazy as LT
import qualified Data.Text.Unsafe as U
import Data.Validity

-- | A text is valid if the internal structure is consistent.
instance Validity ST.Text where
  validate t@(ST.Text arr off len) =
    mconcat
      [ check (len >= 0) "The length is positive.",
        check (off >= 0) "The offset is positive.",
        check
          ( let c = A.unsafeIndex arr off
             in len == 0 || c < 0xDC00 || c > 0xDFFF
          )
          "The offset character is valid UTF16.",
        -- It contains a valid UTF16
        check
          ( (== (Right t :: Either E.UnicodeException ST.Text)) $
              U.unsafeDupablePerformIO
                . try
                . evaluate
                . E.decodeUtf16LEWith E.strictDecode
                . LB.toStrict
                . SBB.toLazyByteString
                . mconcat
                . map SBB.word16LE
                $ A.toList arr off len
          )
          "The bytes can correctly be decoded as UTF16."
      ]

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
