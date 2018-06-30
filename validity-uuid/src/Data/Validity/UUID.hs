{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.UUID where

import Data.Validity

import Data.UUID

-- | A 'UUID' is valid according to the contained words
instance Validity UUID where
    validate = delve "words" . toWords
