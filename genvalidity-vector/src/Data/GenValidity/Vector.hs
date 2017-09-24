{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.Vector where
#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$>))
#endif
import Data.GenValidity
import Data.Validity.Vector ()

import Data.Vector (Vector)
import qualified Data.Vector as V

instance GenUnchecked v => GenUnchecked (Vector v) where
    genUnchecked = V.fromList <$> genUnchecked
    shrinkUnchecked = fmap V.fromList . shrinkUnchecked . V.toList

instance GenValid v => GenValid (Vector v) where
    genValid = V.fromList <$> genValid

instance GenInvalid v => GenInvalid (Vector v) where
    genInvalid = V.fromList <$> genInvalid
