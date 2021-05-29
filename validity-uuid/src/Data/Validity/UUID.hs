{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.UUID where

import Data.UUID
import Data.Validity

-- | A 'UUID' is valid according to the contained words
instance Validity UUID where
  validate = delve "words" . toWords
