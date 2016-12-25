{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Validity.Operations.Associativity
    ( associativeOnGens
    , associativeOnValids
    , associative
    , associativeOnArbitrary
    ) where

import Data.GenValidity

import Test.Hspec
import Test.QuickCheck

associativeOnGens
    :: (Show a, Eq a)
    => (a -> a -> a) -> Gen (a, a, a) -> Property
associativeOnGens op gen =
    forAll gen $ \(a, b, c) ->
        ((a `op` b) `op` c) `shouldBe` (a `op` (b `op` c))

associativeOnValids
    :: (Show a, Eq a, GenValidity a)
    => (a -> a -> a) -> Property
associativeOnValids op = associativeOnGens op genValid

associative
    :: (Show a, Eq a, GenValidity a)
    => (a -> a -> a) -> Property
associative op = associativeOnGens op genUnchecked

-- |
--
-- prop> associativeOnArbitrary ((+) :: Int -> Int -> Int)
associativeOnArbitrary
    :: (Show a, Eq a, Arbitrary a)
    => (a -> a -> a) -> Property
associativeOnArbitrary op = associativeOnGens op arbitrary
