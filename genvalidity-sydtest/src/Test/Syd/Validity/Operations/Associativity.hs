{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Validity.Operations.Associativity
  ( associativeOnGens,
    associative,
    associativeOnArbitrary,
  )
where

import Data.GenValidity
import Test.QuickCheck
import Test.Syd

-- |
--
-- \[
--     Associative(\star)
--     \quad\equiv\quad
--     \forall a, b, c:
--     (a \star b) \star c = a \star (b \star c)
-- \]
associativeOnGens ::
  (Show a, Eq a) =>
  (a -> a -> a) ->
  Gen (a, a, a) ->
  ((a, a, a) -> [(a, a, a)]) ->
  Property
associativeOnGens op gen s =
  forAllShrink gen s $ \(a, b, c) ->
    ((a `op` b) `op` c) `shouldBe` (a `op` (b `op` c))

-- |
--
-- prop> associative ((*) :: Int -> Int -> Int)
-- prop> associative ((+) :: Int -> Int -> Int)
associative :: (Show a, Eq a, GenValid a) => (a -> a -> a) -> Property
associative op = associativeOnGens op genValid shrinkValid

-- |
--
-- prop> associativeOnArbitrary ((*) :: Int -> Int -> Int)
-- prop> associativeOnArbitrary ((+) :: Int -> Int -> Int)
associativeOnArbitrary ::
  (Show a, Eq a, Arbitrary a) => (a -> a -> a) -> Property
associativeOnArbitrary op = associativeOnGens op arbitrary shrink
