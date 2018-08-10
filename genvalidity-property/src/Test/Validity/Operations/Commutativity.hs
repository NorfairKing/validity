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
       (Show a, Eq a)
    => (a -> a -> a)
    -> Gen (a, a)
    -> ((a, a) -> [(a, a)])
    -> Property
commutativeOnGens op gen s =
    forAllShrink gen s $ \(a, b) -> (a `op` b) `shouldBe` (b `op` a)

-- |
--
-- prop> commutativeOnValids ((+) :: Double -> Double -> Double)
-- prop> commutativeOnValids ((*) :: Double -> Double -> Double)
commutativeOnValids :: (Show a, Eq a, GenValid a) => (a -> a -> a) -> Property
commutativeOnValids op = commutativeOnGens op genValid shrinkValid

-- |
--
-- prop> commutative ((+) :: Int -> Int -> Int)
-- prop> commutative ((*) :: Int -> Int -> Int)
commutative :: (Show a, Eq a, GenUnchecked a) => (a -> a -> a) -> Property
commutative op = commutativeOnGens op genUnchecked shrinkUnchecked

-- |
--
-- prop> commutativeOnArbitrary ((+) :: Int -> Int -> Int)
-- prop> commutativeOnArbitrary ((*) :: Int -> Int -> Int)
commutativeOnArbitrary ::
       (Show a, Eq a, Arbitrary a) => (a -> a -> a) -> Property
commutativeOnArbitrary op = commutativeOnGens op arbitrary shrink
