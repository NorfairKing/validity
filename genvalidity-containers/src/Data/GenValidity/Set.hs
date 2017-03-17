{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.Set where

#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$>))
#endif


import Data.GenValidity
import Data.Validity.Set ()

import Data.Set (Set)
import qualified Data.Set as S

instance (Ord v, GenUnchecked v) =>
         GenUnchecked (Set v) where
    genUnchecked = S.fromList <$> genUnchecked

instance (Ord v, GenValid v) =>
         GenValid (Set v) where
    genValid = S.fromList <$> genValid

instance (Ord v, GenInvalid v) =>
         GenInvalid (Set v) where
    genInvalid = S.fromList <$> genInvalid
