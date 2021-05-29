{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Sequence
  ( genSeqOf,
  )
where

#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$>))
#endif
import Data.Foldable (toList)
import Data.GenValidity
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Validity.Sequence ()
import Test.QuickCheck

instance GenUnchecked v => GenUnchecked (Seq v) where
  genUnchecked = genSeqOf genUnchecked
  shrinkUnchecked = fmap S.fromList . shrinkUnchecked . toList

instance GenValid v => GenValid (Seq v) where
  genValid = genSeqOf genValid
  shrinkValid = fmap S.fromList . shrinkValid . toList

instance (GenUnchecked v, GenInvalid v) => GenInvalid (Seq v) where
  shrinkInvalid = fmap S.fromList . shrinkInvalid . toList

genSeqOf :: Gen v -> Gen (Seq v)
genSeqOf g = S.fromList <$> genListOf g
