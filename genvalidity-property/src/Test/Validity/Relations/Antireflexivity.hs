{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Validity.Relations.Antireflexivity
  ( antireflexiveOnElem,
    antireflexivityOnGen,
    antireflexivity,
    antireflexivityOnArbitrary,
  )
where

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
  -- | A relation
  (a -> a -> Bool) ->
  -- | An element
  a ->
  Bool
antireflexiveOnElem func a = not $ func a a

antireflexivityOnGen ::
  Show a => (a -> a -> Bool) -> Gen a -> (a -> [a]) -> Property
antireflexivityOnGen func gen s = forAllShrink gen s $ antireflexiveOnElem func

-- |
--
-- prop> antireflexivity ((<) :: Int -> Int -> Bool)
-- prop> antireflexivity ((/=) :: Int -> Int -> Bool)
-- prop> antireflexivity ((>) :: Int -> Int -> Bool)
antireflexivity :: (Show a, GenValid a) => (a -> a -> Bool) -> Property
antireflexivity func = antireflexivityOnGen func genValid shrinkValid

-- |
--
-- prop> antireflexivityOnArbitrary ((<) :: Int -> Int -> Bool)
-- prop> antireflexivityOnArbitrary ((/=) :: Int -> Int -> Bool)
-- prop> antireflexivityOnArbitrary ((>) :: Int -> Int -> Bool)
antireflexivityOnArbitrary ::
  (Show a, Arbitrary a) => (a -> a -> Bool) -> Property
antireflexivityOnArbitrary func = antireflexivityOnGen func arbitrary shrink
