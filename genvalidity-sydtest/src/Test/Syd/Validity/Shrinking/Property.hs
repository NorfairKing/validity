{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for shrinking functions
module Test.Syd.Validity.Shrinking.Property
  ( shrinkPreservesValidOnGenValid,
    shrinkValidPreservesValid,
    shrinkingStaysValid,
    shrinkingStaysValidWithLimit,
    shrinkingPreserves,
    shrinkingPreservesWithLimit,
    shrinkDoesNotShrinkToItself,
    shrinkDoesNotShrinkToItselfWithLimit,
    shrinkDoesNotShrinkToItselfOnValid,
    shrinkDoesNotShrinkToItselfOnValidWithLimit,
    doesNotShrinkToItself,
    doesNotShrinkToItselfWithLimit,
  )
where

import Data.GenValidity
import Test.QuickCheck

-- |
--
-- prop> shrinkPreservesValidOnGenValid ((:[]) :: Int -> [Int])
shrinkPreservesValidOnGenValid ::
  forall a.
  (Show a, GenValid a) =>
  (a -> [a]) ->
  Property
shrinkPreservesValidOnGenValid = shrinkingStaysValid genValid

-- |
--
-- prop> shrinkValidPreservesValid (pure 5 :: Gen Rational)
shrinkValidPreservesValid ::
  forall a.
  (Show a, GenValid a) =>
  Gen a ->
  Property
shrinkValidPreservesValid gen = shrinkingStaysValid gen shrinkValid

-- |
--
-- prop> shrinkingStaysValid (pure 5 :: Gen Double) (\d -> [d - 1, d - 2])
shrinkingStaysValid ::
  forall a.
  (Show a, Validity a) =>
  Gen a ->
  (a -> [a]) ->
  Property
shrinkingStaysValid gen s = shrinkingPreserves gen s isValid

-- |
--
-- prop> shrinkingStaysValidWithLimit (pure 5 :: Gen Double) (\d -> [d - 1, read "NaN"]) 1
shrinkingStaysValidWithLimit ::
  forall a.
  (Show a, Validity a) =>
  Gen a ->
  (a -> [a]) ->
  Int ->
  Property
shrinkingStaysValidWithLimit gen s l =
  shrinkingPreservesWithLimit gen s l isValid

-- |
--
-- prop> shrinkingPreserves (pure 5 :: Gen Int) (:[]) (== 5)
shrinkingPreserves ::
  forall a.
  (Show a) =>
  Gen a ->
  (a -> [a]) ->
  (a -> Bool) ->
  Property
shrinkingPreserves gen s p = forAll gen $ \d -> not (p d) || all p (s d)

-- |
--
-- prop> shrinkingPreservesWithLimit (pure 4) (:[]) 100 (== 4)
shrinkingPreservesWithLimit ::
  forall a.
  (Show a) =>
  Gen a ->
  (a -> [a]) ->
  Int ->
  (a -> Bool) ->
  Property
shrinkingPreservesWithLimit gen s l p =
  forAll gen $ \d -> not (p d) || all p (take l $ s d)

-- |
--
-- prop> shrinkDoesNotShrinkToItself (shrinkValid :: Double -> [Double])
shrinkDoesNotShrinkToItself ::
  forall a.
  (Show a, Eq a, GenValid a) =>
  (a -> [a]) ->
  Property
shrinkDoesNotShrinkToItself = doesNotShrinkToItself genValid

-- |
--
-- prop> shrinkDoesNotShrinkToItselfWithLimit (shrinkValid :: Double -> [Double]) 100
shrinkDoesNotShrinkToItselfWithLimit ::
  forall a.
  (Show a, Eq a, GenValid a) =>
  (a -> [a]) ->
  Int ->
  Property
shrinkDoesNotShrinkToItselfWithLimit =
  doesNotShrinkToItselfWithLimit genValid

-- |
--
-- prop> shrinkDoesNotShrinkToItselfOnValid (shrinkValid ::  Rational -> [Rational])
shrinkDoesNotShrinkToItselfOnValid ::
  forall a.
  (Show a, Eq a, GenValid a) =>
  (a -> [a]) ->
  Property
shrinkDoesNotShrinkToItselfOnValid = doesNotShrinkToItself genValid

-- |
--
-- prop> shrinkDoesNotShrinkToItselfOnValidWithLimit (shrinkValid :: Rational -> [Rational]) 100
shrinkDoesNotShrinkToItselfOnValidWithLimit ::
  forall a.
  (Show a, Eq a, GenValid a) =>
  (a -> [a]) ->
  Int ->
  Property
shrinkDoesNotShrinkToItselfOnValidWithLimit =
  doesNotShrinkToItselfWithLimit genValid

-- |
--
-- prop> doesNotShrinkToItself (pure 5 :: Gen Double) shrinkValid
doesNotShrinkToItself ::
  forall a.
  (Show a, Eq a) =>
  Gen a ->
  (a -> [a]) ->
  Property
doesNotShrinkToItself gen s = forAll gen $ \a -> notElem a $ s a

-- |
--
-- prop> doesNotShrinkToItselfWithLimit (pure 5 :: Gen Double) shrinkValid 100
doesNotShrinkToItselfWithLimit ::
  forall a.
  (Show a, Eq a) =>
  Gen a ->
  (a -> [a]) ->
  Int ->
  Property
doesNotShrinkToItselfWithLimit gen s l =
  forAll gen $ \a -> notElem a $ take l $ s a
