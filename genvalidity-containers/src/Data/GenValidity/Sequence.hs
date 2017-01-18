module Data.GenValidity.Sequence where

import Data.GenValidity
import Data.Validity.Sequence ()

import Data.Sequence (Seq)
import qualified Data.Sequence as S

instance (Ord v, GenUnchecked v) =>
         GenUnchecked (Seq v) where
    genUnchecked = S.fromList <$> genUnchecked

instance (Ord v, GenValid v) =>
         GenValid (Seq v) where
    genValid = S.fromList <$> genValid

instance (Ord v, GenInvalid v) =>
         GenInvalid (Seq v) where
    genInvalid = S.fromList <$> genInvalid
