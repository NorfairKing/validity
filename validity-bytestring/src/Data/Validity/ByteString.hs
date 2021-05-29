{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.ByteString where

import qualified Data.ByteString as SB
import qualified Data.ByteString.Internal as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Internal as LB
import qualified Data.ByteString.Short as Short
import Data.Validity

-- | A 'ByteString' is NOT trivially valid.
--
-- The offset and the length both need to be positive.
-- Note that the length does not need to be greater than, or equal to, the offset.
--
-- TODO there's nothing we can do about the foreign pointer, I think?
instance Validity SB.ByteString where
  validate (SB.PS _ off len) =
    mconcat
      [ delve "offset" off,
        delve "length" len,
        declare "The offset is positive" $ off >= 0,
        declare "The length is positive" $ len >= 0
      ]

-- | A lazy 'ByteString' is valid according to its chunks.
instance Validity LB.ByteString where
  validate = go 0
    where
      go :: Int -> LB.ByteString -> Validation
      go _ LB.Empty = valid
      go i (LB.Chunk sb lb) =
        mconcat [delve (unwords ["Chunk number", show i]) sb, go (i + 1) lb]

-- | Trivially valid
--
-- My guess is that short bytestrings are not trivially valid but there is no way to access the internals.
instance Validity Short.ShortByteString where
  validate = trivialValidation
