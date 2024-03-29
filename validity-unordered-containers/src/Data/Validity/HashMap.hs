{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.HashMap where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Validity

-- | A 'HashMap' of things is valid if all the keys and values are valid.
--
-- The 'unordered-containers' package does not export any more functionality
-- concerning a 'HashMap', so no more accurate validity instance can be made.
instance (Validity k, Validity v) => Validity (HashMap k v) where
  validate = delve "HashMap elements" . HM.toList
