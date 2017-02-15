{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Validity.Text where

import Data.Validity

import qualified Data.Text as T -- For the show instance
import qualified Data.Text.Array as A
import Data.Text.Internal (Text(..))
import qualified Data.Text.Internal.Encoding.Utf16 as E

-- | A text is valid if the internal structure is consistent.
instance Validity Text where
    isValid t@(Text arr off len) =
        let c = A.unsafeIndex arr off
        in and
               [ len >= 0
               , off >= 0
               , len == 0 || c < 0xDC00 || c > 0xDFFF
               , let ws = A.toList arr off len -- Checking for failures while decoding the UTFX
                 in all E.validate1 ws && all (uncurry E.validate2) (tupsOf ws)
               ]
      where
        tupsOf :: [a] -> [(a, a)]
        tupsOf [] = []
        tupsOf [_] = []
        tupsOf (a:b:rs) = (a, b) : tupsOf rs
