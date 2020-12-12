{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for shrinking functions
module Test.Syd.Validity.Shrinking.Property
    ( shrinkPreservesValidOnGenValid
    , shrinkPreservesInvalidOnGenInvalid
    , shrinkValidPreservesValid
    , shrinkInvalidPreservesInvalid
    , shrinkingStaysValid
    , shrinkingStaysValidWithLimit
    , shrinkingStaysInvalid
    , shrinkingPreserves
    , shrinkingPreservesWithLimit
    , shrinkDoesNotShrinkToItself
    , shrinkDoesNotShrinkToItselfWithLimit
    , shrinkDoesNotShrinkToItselfOnValid
    , shrinkDoesNotShrinkToItselfOnValidWithLimit
    , shrinkDoesNotShrinkToItselfOnInvalid
    , shrinkDoesNotShrinkToItselfOnInvalidWithLimit
    , doesNotShrinkToItself
    , doesNotShrinkToItselfWithLimit
    ) where

import Data.GenValidity

import Test.QuickCheck

-- |
--
-- prop> shrinkPreservesValidOnGenValid ((:[]) :: Int -> [Int])
shrinkPreservesValidOnGenValid ::
       forall a. (Show a, GenValid a)
    => (a -> [a])
    -> Property
shrinkPreservesValidOnGenValid = shrinkingStaysValid genValid

-- |
--
-- prop> shrinkPreservesInvalidOnGenInvalid ((:[]) :: Rational -> [Rational])
shrinkPreservesInvalidOnGenInvalid ::
       forall a. (Show a, GenInvalid a)
    => (a -> [a])
    -> Property
shrinkPreservesInvalidOnGenInvalid = shrinkingStaysValid genInvalid

-- |
--
-- prop> shrinkValidPreservesValid (pure 5 :: Gen Rational)
shrinkValidPreservesValid ::
       forall a. (Show a, GenValid a)
    => Gen a
    -> Property
shrinkValidPreservesValid gen = shrinkingStaysValid gen shrinkValid

-- |
--
shrinkInvalidPreservesInvalid ::
       forall a. (Show a, GenInvalid a)
    => Gen a
    -> Property
shrinkInvalidPreservesInvalid gen = shrinkingStaysValid gen shrinkInvalid

-- |
--
-- prop> shrinkingStaysValid (pure 5 :: Gen Double) (\d -> [d - 1, d - 2])
shrinkingStaysValid ::
       forall a. (Show a, Validity a)
    => Gen a
    -> (a -> [a])
    -> Property
shrinkingStaysValid gen s = shrinkingPreserves gen s isValid

-- |
--
-- prop> shrinkingStaysValidWithLimit (pure 5 :: Gen Double) (\d -> [d - 1, read "NaN"]) 1
shrinkingStaysValidWithLimit ::
       forall a. (Show a, Validity a)
    => Gen a
    -> (a -> [a])
    -> Int
    -> Property
shrinkingStaysValidWithLimit gen s l =
    shrinkingPreservesWithLimit gen s l isValid

-- |
--
-- prop> shrinkingStaysInvalid (pure (1/0) :: Gen Double) (:[])
shrinkingStaysInvalid ::
       forall a. (Show a, Validity a)
    => Gen a
    -> (a -> [a])
    -> Property
shrinkingStaysInvalid gen s = shrinkingPreserves gen s isInvalid

-- |
--
-- prop> shrinkingPreserves (pure 5 :: Gen Int) (:[]) (== 5)
shrinkingPreserves ::
       forall a. Show a
    => Gen a
    -> (a -> [a])
    -> (a -> Bool)
    -> Property
shrinkingPreserves gen s p = forAll gen $ \d -> not (p d) || all p (s d)

-- |
--
-- prop> shrinkingPreservesWithLimit (pure 4) (:[]) 100 (== 4)
shrinkingPreservesWithLimit ::
       forall a. Show a
    => Gen a
    -> (a -> [a])
    -> Int
    -> (a -> Bool)
    -> Property
shrinkingPreservesWithLimit gen s l p =
    forAll gen $ \d -> not (p d) || all p (take l $ s d)

-- |
--
-- prop> shrinkDoesNotShrinkToItself (shrinkUnchecked :: Double -> [Double])
shrinkDoesNotShrinkToItself ::
       forall a. (Show a, Eq a, GenUnchecked a)
    => (a -> [a])
    -> Property
shrinkDoesNotShrinkToItself = doesNotShrinkToItself genUnchecked

-- |
--
-- prop> shrinkDoesNotShrinkToItselfWithLimit (shrinkUnchecked :: Double -> [Double]) 100
shrinkDoesNotShrinkToItselfWithLimit ::
       forall a. (Show a, Eq a, GenUnchecked a)
    => (a -> [a])
    -> Int
    -> Property
shrinkDoesNotShrinkToItselfWithLimit =
    doesNotShrinkToItselfWithLimit genUnchecked

-- |
--
-- prop> shrinkDoesNotShrinkToItselfOnValid (shrinkValid ::  Rational -> [Rational])
shrinkDoesNotShrinkToItselfOnValid ::
       forall a. (Show a, Eq a, GenValid a)
    => (a -> [a])
    -> Property
shrinkDoesNotShrinkToItselfOnValid = doesNotShrinkToItself genValid

-- |
--
-- prop> shrinkDoesNotShrinkToItselfOnValidWithLimit (shrinkValid :: Rational -> [Rational]) 100
shrinkDoesNotShrinkToItselfOnValidWithLimit ::
       forall a. (Show a, Eq a, GenValid a)
    => (a -> [a])
    -> Int
    -> Property
shrinkDoesNotShrinkToItselfOnValidWithLimit =
    doesNotShrinkToItselfWithLimit genValid

-- |
--
-- prop> shrinkDoesNotShrinkToItselfOnInvalid (shrinkInvalid :: Rational -> [Rational])
shrinkDoesNotShrinkToItselfOnInvalid ::
       forall a. (Show a, Eq a, GenInvalid a)
    => (a -> [a])
    -> Property
shrinkDoesNotShrinkToItselfOnInvalid = doesNotShrinkToItself genInvalid

-- |
--
-- prop> shrinkDoesNotShrinkToItselfOnInvalidWithLimit (shrinkInvalid :: Rational -> [Rational]) 100
shrinkDoesNotShrinkToItselfOnInvalidWithLimit ::
       forall a. (Show a, Eq a, GenInvalid a)
    => (a -> [a])
    -> Int
    -> Property
shrinkDoesNotShrinkToItselfOnInvalidWithLimit =
    doesNotShrinkToItselfWithLimit genInvalid

-- |
--
-- prop> doesNotShrinkToItself (pure 5 :: Gen Double) shrinkUnchecked
doesNotShrinkToItself ::
       forall a. (Show a, Eq a)
    => Gen a
    -> (a -> [a])
    -> Property
doesNotShrinkToItself gen s = forAll gen $ \a -> all (/= a) $ s a

-- |
--
-- prop> doesNotShrinkToItselfWithLimit (pure 5 :: Gen Double) shrinkUnchecked 100
doesNotShrinkToItselfWithLimit ::
       forall a. (Show a, Eq a)
    => Gen a
    -> (a -> [a])
    -> Int
    -> Property
doesNotShrinkToItselfWithLimit gen s l =
    forAll gen $ \a -> all (/= a) $ take l $ s a
