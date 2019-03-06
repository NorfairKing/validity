{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.Sequence where
#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$>))
#endif
import Data.Foldable (toList)
import Data.GenValidity
import Data.Validity.Sequence ()

import Data.Sequence (Seq)
import qualified Data.Sequence as S

instance GenUnchecked v => GenUnchecked (Seq v) where
    genUnchecked = S.fromList <$> genUnchecked
    shrinkUnchecked = fmap S.fromList . shrinkUnchecked . toList

instance GenValid v => GenValid (Seq v) where
    genValid = S.fromList <$> genValid
    shrinkValid = fmap S.fromList . shrinkValid . toList

instance (GenUnchecked v, GenInvalid v) => GenInvalid (Seq v) where
    genInvalid = S.fromList <$> genInvalid
    shrinkInvalid = fmap S.fromList . shrinkInvalid . toList
