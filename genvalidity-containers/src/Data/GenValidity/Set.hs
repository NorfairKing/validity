{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Set
  ( genSetOf,
    shrinkSetOf,
    genSeperate,
    genSeperateFor,
    genSeperateForNE,
    genValidSeperateFor,
    genValidSeperateForNE,
  )
where

import Data.Containers.ListUtils
import Data.GenValidity
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as S
import Data.Validity.Set ()
import Test.QuickCheck

instance (Ord v, GenValid v) => GenValid (Set v) where
  genValid = genSetOf genValid
  shrinkValid = shrinkSetOf shrinkValid

genSetOf :: Ord v => Gen v -> Gen (Set v)
genSetOf g = S.fromList <$> genListOf g

shrinkSetOf :: Ord v => (v -> [v]) -> Set v -> [Set v]
shrinkSetOf shrinker = fmap S.fromList . shrinkList shrinker . S.toList

genValidSeperateFor :: (GenValid b, Eq b) => [a] -> Gen [(b, a)]
genValidSeperateFor = genSeperateFor genValid

genValidSeperateForNE :: (GenValid b, Eq b) => NonEmpty a -> Gen (NonEmpty (b, a))
genValidSeperateForNE = genSeperateForNE genValid

genSeperate :: Ord a => Gen a -> Gen [a]
genSeperate g = nubOrd <$> genListOf g

-- TODO these two can likely be optimised
genSeperateFor :: Eq b => Gen b -> [a] -> Gen [(b, a)]
genSeperateFor _ [] = pure []
genSeperateFor g (a : as) = NE.toList <$> genSeperateForNE g (a :| as)

genSeperateForNE :: Eq b => Gen b -> NonEmpty a -> Gen (NonEmpty (b, a))
genSeperateForNE g (a :| as) = do
  restTups <- genSeperateFor g as
  b <- g `suchThat` (`notElem` map fst restTups)
  pure ((b, a) :| restTups)
