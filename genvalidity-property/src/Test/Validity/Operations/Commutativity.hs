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

-- |
--
-- \[
--     Commutative(\star)
--     \quad\equiv\quad
--     \forall a, b:
--     a \star b = b \star a
-- \]
commutativeOnGens ::
       (Show a, Show b, Eq b)
    => (a -> a -> b)
    -> Gen (a, a)
    -> ((a, a) -> [(a, a)])
    -> Property
commutativeOnGens op gen s =
    forAllShrink gen s $ \(a, b) -> (a `op` b) `shouldBe` (b `op` a)

-- |
--
-- prop> commutativeOnValids ((+) :: Rational -> Rational -> Rational)
-- prop> commutativeOnValids ((*) :: Rational -> Rational -> Rational)
commutativeOnValids ::
          (Show a, Show b, Eq b, GenValid a)
       => (a -> a -> b) -> Property
commutativeOnValids op = commutativeOnGens op genValid shrinkValid

-- |
--
-- prop> commutative ((+) :: Int -> Int -> Int)
-- prop> commutative ((*) :: Int -> Int -> Int)
commutative ::
          (Show a, Show b, Eq b, GenUnchecked a)
       => (a -> a -> b) -> Property
commutative op = commutativeOnGens op genUnchecked shrinkUnchecked

-- |
--
-- prop> commutativeOnArbitrary ((+) :: Int -> Int -> Int)
-- prop> commutativeOnArbitrary ((*) :: Int -> Int -> Int)
-- commutativeOnArbitrary ::
--        (Show a, Eq a, Arbitrary a) => (a -> a -> a) -> Property
commutativeOnArbitrary ::
          (Show a, Show b, Eq b, Arbitrary a)
       => (a -> a -> b) -> Property
commutativeOnArbitrary op = commutativeOnGens op arbitrary shrink
