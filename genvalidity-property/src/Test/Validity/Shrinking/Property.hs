{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for shrinking functions
module Test.Validity.Shrinking.Property
    ( shrinkPreservesValidOnGenValid
    , shrinkPreservesInvalidOnGenInvalid
    , shrinkValidPreservesValid
    , shrinkInvalidPreservesInvalid
    , shrinkingStaysValid
    , shrinkingStaysInvalid
    , shrinkingPreserves
    ) where

import Data.GenValidity

import Test.QuickCheck

-- |
--
-- prop> shrinkPreservesValidOnGenValid (:[])
shrinkPreservesValidOnGenValid ::
       forall a. (Show a, GenValid a)
    => (a -> [a])
    -> Property
shrinkPreservesValidOnGenValid = shrinkingStaysValid genValid

-- |
--
-- prop> shrinkPreservesInvalidOnGenInvalid (:[])
shrinkPreservesInvalidOnGenInvalid ::
       forall a. (Show a, GenInvalid a)
    => (a -> [a])
    -> Property
shrinkPreservesInvalidOnGenInvalid = shrinkingStaysValid genInvalid

-- |
--
-- prop> shrinkValidPreservesValid (pure 5)
shrinkValidPreservesValid ::
       forall a. (Show a, GenValid a)
    => Gen a
    -> Property
shrinkValidPreservesValid gen = shrinkingStaysValid gen shrinkValid

-- |
--
-- prop> shrinkInvalidPreservesInvalid (pure (1/0) :: Gen Double)
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
-- prop> shrinkingStaysInvalid (pure (1/0) :: Gen Double) (:[])
shrinkingStaysInvalid ::
       forall a. (Show a, Validity a)
    => Gen a
    -> (a -> [a])
    -> Property
shrinkingStaysInvalid gen s = shrinkingPreserves gen s isInvalid

-- |
--
-- prop> shrinkingPreserves (pure 5) (:[]) (== 5)
shrinkingPreserves ::
       forall a. Show a
    => Gen a
    -> (a -> [a])
    -> (a -> Bool)
    -> Property
shrinkingPreserves gen s p = forAll gen $ \d -> not (p d) || all p (s d)
