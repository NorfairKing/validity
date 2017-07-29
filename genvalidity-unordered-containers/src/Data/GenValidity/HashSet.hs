{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.HashSet where
#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$>))
#endif
import Data.GenValidity
import Data.Validity.HashSet ()

import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)

instance (Hashable v, Eq v, GenUnchecked v) => GenUnchecked (HashSet v) where
    genUnchecked = HS.fromList <$> genUnchecked

instance (Hashable v, Eq v, GenValid v) => GenValid (HashSet v) where
    genValid = HS.fromList <$> genValid

instance (Hashable v,Eq v, GenInvalid v) => GenInvalid (HashSet v) where
    genInvalid = HS.fromList <$> genInvalid
