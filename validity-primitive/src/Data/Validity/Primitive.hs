{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.Primitive where

import qualified Data.Primitive.Array as A
import Data.Validity

#if MIN_VERSION_primitive(0,6,2)
instance Validity a => Validity (A.Array a) where
    validate = foldMap validate
#else
instance Validity a => Validity (A.Array a) where
    validate xs =
        let n = I# (sizeofArray# (array# xs))
         in delve "Primitive array" . validate $ indexArray xs <$> [0 .. n - 1]
#endif
