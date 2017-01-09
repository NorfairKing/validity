{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Validity.Operations.Commutativity
    ( commutativeOnGens
    , commutativeOnValids
    , commutative
    , commutativeOnArbitrary
    ) where

import Data.GenValidity

import Test.Hspec
import Test.QuickCheck

commutativeOnGens
    :: (Show a, Eq a)
    => (a -> a -> a) -> Gen (a, a) -> Property
commutativeOnGens op gen =
    forAll gen $ \(a, b) -> (a `op` b) `shouldBe` (b `op` a)

-- |
--
-- prop> commutative ((+) @Double)
-- prop> commutative ((*) @Double)
commutativeOnValids
    :: (Show a, Eq a, GenValid a)
    => (a -> a -> a) -> Property
commutativeOnValids op = commutativeOnGens op genValid

-- |
--
-- prop> commutative ((+) @Int)
-- prop> commutative ((*) @Int)
commutative
    :: (Show a, Eq a, GenUnchecked a)
    => (a -> a -> a) -> Property
commutative op = commutativeOnGens op genUnchecked

-- |
--
-- prop> commutativeOnArbitrary ((+) @Int)
-- prop> commutativeOnArbitrary ((*) @Int)
commutativeOnArbitrary
    :: (Show a, Eq a, Arbitrary a)
    => (a -> a -> a) -> Property
commutativeOnArbitrary op = commutativeOnGens op arbitrary
