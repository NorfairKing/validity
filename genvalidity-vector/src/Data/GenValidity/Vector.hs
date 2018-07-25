{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

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

instance GenInvalid v => GenInvalid (V.Vector v) where
    genInvalid = V.fromList <$> genInvalid

instance (SV.Storable e, GenUnchecked e) => GenUnchecked (SV.Vector e) where
    genUnchecked = SV.fromList <$> genUnchecked
    shrinkUnchecked = fmap SV.fromList . shrinkUnchecked . SV.toList

instance (SV.Storable e, GenValid e) => GenValid (SV.Vector e) where
    genValid = SV.fromList <$> genValid
