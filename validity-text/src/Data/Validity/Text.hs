{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Data.Validity.Text where

import Control.Exception (evaluate, try)

import Data.Validity

import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import Data.Text ()
import qualified Data.Text.Array as A
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as E
import Data.Text.Internal (Text(..))
import qualified Data.Text.Unsafe as U
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif
-- | A text is valid if the internal structure is consistent.
instance Validity Text where
    validate t@(Text arr off len) =
        mconcat
            [ check (len >= 0) "The length is positive."
            , check (off >= 0) "The offset is positive."
            , check
                  (let c = A.unsafeIndex arr off
                    in len == 0 || c < 0xDC00 || c > 0xDFFF)
                  "The offset character is valid UTF16."
                 -- It contains a valid UTF16
            , check ((== (Right t :: Either E.UnicodeException Text)) $
               U.unsafeDupablePerformIO .
               try .
               evaluate .
               E.decodeUtf16LEWith E.strictDecode .
               LB.toStrict . SBB.toLazyByteString . mconcat . map SBB.word16LE $
               A.toList arr off len)
              "The bytes can correctly be decoded as UTF16."
            ]
