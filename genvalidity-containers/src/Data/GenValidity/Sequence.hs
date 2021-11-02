{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Sequence
  ( genSeqOf,
  )
where

import Data.Foldable (toList)
import Data.GenValidity
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Validity.Sequence ()
import Test.QuickCheck

instance GenValid v => GenValid (Seq v) where
  genValid = genSeqOf genValid
  shrinkValid = fmap S.fromList . shrinkValid . toList

genSeqOf :: Gen v -> Gen (Seq v)
genSeqOf g = S.fromList <$> genListOf g
