{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.DNonEmpty () where

import Data.DList.DNonEmpty (DNonEmpty)
import qualified Data.DList.DNonEmpty as DNonEmpty
import Data.Validity (Validity (..))

-- | A 'DNonEmpty' of things is valid if a 'NonEmpty' of the same things is valid
instance (Validity v) => Validity (DNonEmpty v) where
  validate = validate . DNonEmpty.toNonEmpty
