{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Validity.Relations.Antireflexivity
    ( antireflexiveOnElem
    , antireflexivityOnGen
    , antireflexivityOnValid
    , antireflexivity
    , antireflexivityOnArbitrary
    ) where

import Data.GenValidity

import Test.QuickCheck

-- |
--
-- \[
--   Antireflexive(\prec)
--   \quad\equiv\quad
--   \forall a: \neg (a \prec a)
-- \]
antireflexiveOnElem ::
       (a -> a -> Bool) -- ^ A relation
    -> a -- ^ An element
    -> Bool
antireflexiveOnElem func a = not $ func a a

antireflexivityOnGen ::
       Show a => (a -> a -> Bool) -> Gen a -> (a -> [a]) -> Property
antireflexivityOnGen func gen s = forAllShrink gen s $ antireflexiveOnElem func

-- |
--
-- prop> antireflexivityOnValid ((<) :: Rational -> Rational -> Bool)
-- prop> antireflexivityOnValid ((/=) :: Rational -> Rational -> Bool)
-- prop> antireflexivityOnValid ((>) :: Rational -> Rational -> Bool)
antireflexivityOnValid :: (Show a, GenValid a) => (a -> a -> Bool) -> Property
antireflexivityOnValid func = antireflexivityOnGen func genValid shrinkValid

-- |
--
-- prop> antireflexivity ((<) :: Int -> Int -> Bool)
-- prop> antireflexivity ((/=) :: Int -> Int -> Bool)
-- prop> antireflexivity ((>) :: Int -> Int -> Bool)
antireflexivity :: (Show a, GenUnchecked a) => (a -> a -> Bool) -> Property
antireflexivity func = antireflexivityOnGen func genUnchecked shrinkUnchecked

-- |
--
-- prop> antireflexivityOnArbitrary ((<) :: Int -> Int -> Bool)
-- prop> antireflexivityOnArbitrary ((/=) :: Int -> Int -> Bool)
-- prop> antireflexivityOnArbitrary ((>) :: Int -> Int -> Bool)
antireflexivityOnArbitrary ::
       (Show a, Arbitrary a) => (a -> a -> Bool) -> Property
antireflexivityOnArbitrary func = antireflexivityOnGen func arbitrary shrink
