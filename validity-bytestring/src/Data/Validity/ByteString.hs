{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.ByteString where

import Data.Validity

import qualified Data.ByteString as SB
import qualified Data.ByteString.Internal as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Internal as LB

-- | A 'ByteString' is trivially valid.
-- TODO there's nothing we can do about the foreign pointer, I think?
instance Validity SB.ByteString where
    validate (SB.PS fptr off len) =
        mconcat
            [ delve "offset" off
            , delve "length" len
            , declare "The offset is positive" $ off >= 0
            , declare "The length is positive" $ len >= 0
            , declare "The length is greater than the offset" $ len >= off
            ]

-- | A lazy 'ByteString' is valid according to its chunks.
instance Validity LB.ByteString where
    validate = go 0
      where
        go :: Int -> LB.ByteString -> Validation
        go _ LB.Empty = valid
        go i (LB.Chunk sb lb) =
            mconcat [delve (unwords ["Chunk number", show i]) sb, go (i + 1) lb]
