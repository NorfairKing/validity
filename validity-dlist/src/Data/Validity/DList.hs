{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.DList () where

import Data.DList (DList)
import qualified Data.DList as DList
import Data.Validity (Validity (..))

-- | A 'DList' of things is valid if list of the same things is valid
instance (Validity v) => Validity (DList v) where
  validate = validate . DList.toList
