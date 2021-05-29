{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Vector where

#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$>))
#endif
import Data.GenValidity
import Data.Validity.Vector ()
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV

instance GenUnchecked v => GenUnchecked (V.Vector v) where
  genUnchecked = V.fromList <$> genUnchecked
  shrinkUnchecked = fmap V.fromList . shrinkUnchecked . V.toList

instance GenValid v => GenValid (V.Vector v) where
  genValid = V.fromList <$> genValid
  shrinkValid = fmap V.fromList . shrinkValid . V.toList

instance (GenUnchecked v, GenInvalid v) => GenInvalid (V.Vector v) where
  genInvalid = V.fromList <$> genInvalid
  shrinkInvalid = fmap V.fromList . shrinkInvalid . V.toList

instance (SV.Storable e, GenUnchecked e) => GenUnchecked (SV.Vector e) where
  genUnchecked = SV.fromList <$> genUnchecked
  shrinkUnchecked = fmap SV.fromList . shrinkUnchecked . SV.toList

instance (SV.Storable e, GenValid e) => GenValid (SV.Vector e) where
  genValid = SV.fromList <$> genValid
  shrinkValid = fmap SV.fromList . shrinkValid . SV.toList
