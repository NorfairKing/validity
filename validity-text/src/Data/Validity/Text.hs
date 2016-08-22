module Data.Validity.Text where

import           Data.Validity

import           Data.Text.Internal (Text(..))
import qualified Data.Text.Array as A

-- | A text is valid if the internal structure is consistent.
instance Validity Text where
    isValid (Text arr off len) =
        let c    = A.unsafeIndex arr off
        in    (len >= 0)
           && (off >= 0)
           -- TODO(syd) also check the length of the byte array if that is
           -- possible. Like this:
           -- && (alen == 0 || len == 0 || off < alen)
           && (len == 0 || c < 0xDC00 || c > 0xDFFF)


