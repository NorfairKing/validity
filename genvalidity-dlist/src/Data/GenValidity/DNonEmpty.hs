{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.DNonEmpty () where

import Data.DList.DNonEmpty (DNonEmpty)
import qualified Data.DList.DNonEmpty as DNonEmpty
import Data.GenValidity (GenValid (..))
import Data.Validity.DNonEmpty ()

instance (GenValid a) => GenValid (DNonEmpty a) where
  genValid = DNonEmpty.fromList <$> genValid
  shrinkValid = fmap DNonEmpty.fromNonEmpty . shrinkValid . DNonEmpty.toNonEmpty
