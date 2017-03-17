{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.Sequence where
#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$>))
#endif
import Data.GenValidity
import Data.Validity.Sequence ()

import Data.Sequence (Seq)
import qualified Data.Sequence as S

instance GenUnchecked v =>
         GenUnchecked (Seq v) where
    genUnchecked = S.fromList <$> genUnchecked

instance GenValid v =>
         GenValid (Seq v) where
    genValid = S.fromList <$> genValid

instance GenInvalid v =>
         GenInvalid (Seq v) where
    genInvalid = S.fromList <$> genInvalid
