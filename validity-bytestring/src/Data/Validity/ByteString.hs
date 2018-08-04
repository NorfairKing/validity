{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.ByteString where

import Data.Validity

import qualified Data.ByteString as SB
import qualified Data.ByteString.Internal as SB

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
