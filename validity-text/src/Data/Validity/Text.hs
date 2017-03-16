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
    isValid t@(Text arr off len) =
        and
            [ len >= 0
            , off >= 0
            , let c = A.unsafeIndex arr off
              in len == 0 || c < 0xDC00 || c > 0xDFFF
                 -- It contains a valid UTF16
            , (== (Right t :: Either E.UnicodeException Text)) $
              U.unsafeDupablePerformIO .
              try .
              evaluate .
              E.decodeUtf16LEWith E.strictDecode .
              LB.toStrict . SBB.toLazyByteString . mconcat . map SBB.word16LE $
              A.toList arr off len
            ]
