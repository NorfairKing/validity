{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Validity.ByteString where

import Data.Validity

import Data.ByteString

-- | A 'ByteString' is trivially valid.
instance Validity ByteString where
    isValid = const True
        -- ^ TODO(syd) Actually check that the offset and length are positive.
