{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Data.Validity.Primitive where

import Data.Validity

import qualified Data.Primitive.Array as A
#if MIN_VERSION_primitive(0,6,2)
instance Validity a => Validity (A.Array a) where
    validate = foldMap validate
#else
instance Validity a => Validity (A.Array a) where
    validate xs =
        let n = I# (sizeofArray# (array# xs))
         in delve "Primitive array" . validate $ indexArray xs <$> [0 .. n - 1]
#endif
