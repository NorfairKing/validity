{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Sequence
  ( genSeqOf,
    shrinkSeqOf,
  )
where

import Data.Foldable (toList)
import Data.GenValidity
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Validity.Sequence ()
import Test.QuickCheck

instance (GenValid v) => GenValid (Seq v) where
  genValid = genSeqOf genValid
  shrinkValid = shrinkSeqOf shrinkValid

genSeqOf :: Gen v -> Gen (Seq v)
genSeqOf g = S.fromList <$> genListOf g

shrinkSeqOf :: (v -> [v]) -> Seq v -> [Seq v]
shrinkSeqOf shrinker = fmap S.fromList . shrinkList shrinker . toList
