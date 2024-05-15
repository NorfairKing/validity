{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Vector where

import Data.GenValidity
import Data.Validity.Vector ()
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV

instance (GenValid v) => GenValid (V.Vector v) where
  genValid = V.fromList <$> genValid
  shrinkValid = fmap V.fromList . shrinkValid . V.toList

instance (SV.Storable e, GenValid e) => GenValid (SV.Vector e) where
  genValid = SV.fromList <$> genValid
  shrinkValid = fmap SV.fromList . shrinkValid . SV.toList
