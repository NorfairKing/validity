{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Validity.ByteString where

import Data.Validity

import Data.ByteString

-- | A 'ByteString' is trivially valid.
instance Validity ByteString where
    isValid = const True
