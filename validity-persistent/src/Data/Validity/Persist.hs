{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.Persist where

import Data.Validity
import Database.Persist

instance Validity (Key a) where
  validate = trivialValidation

instance (Validity a) => Validity (Entity a) where
  validate e = mconcat [delve "entityKey" $ entityKey e, delve "entityVal" $ entityVal e]
