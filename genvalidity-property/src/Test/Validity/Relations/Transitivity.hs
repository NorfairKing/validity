{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Validity.Relations.Transitivity
  ( transitiveOnElems,
    transitivityOnGens,
    transitivity,
    transitivityOnArbitrary,
  )
where

import Data.GenValidity
import Test.QuickCheck
import Test.Validity.Property.Utils

-- |
--
-- \[
--   Transitive(\prec)
--   \quad\equiv\quad
--   \forall a, b, c: ((a \prec b) \wedge (b \prec c)) \Rightarrow (a \prec c)
-- \]
transitiveOnElems ::
  -- | A relation
  (a -> a -> Bool) ->
  a ->
  a ->
  -- | Three elements
  a ->
  Bool
transitiveOnElems func a b c = (func a b && func b c) ===> func a c

transitivityOnGens ::
  Show a => (a -> a -> Bool) -> Gen (a, a, a) -> (a -> [a]) -> Property
transitivityOnGens func gen s =
  forAllShrink gen (shrinkT3 s) $ \(a, b, c) -> transitiveOnElems func a b c

-- |
--
-- prop> transitivity ((>) :: Int -> Int -> Bool)
-- prop> transitivity ((>=) :: Int -> Int -> Bool)
-- prop> transitivity ((==) :: Int -> Int -> Bool)
-- prop> transitivity ((<=) :: Int -> Int -> Bool)
-- prop> transitivity ((<) :: Int -> Int -> Bool)
-- prop> transitivity (Data.List.isPrefixOf :: [Int] -> [Int] -> Bool)
-- prop> transitivity (Data.List.isSuffixOf :: [Int] -> [Int] -> Bool)
-- prop> transitivity (Data.List.isInfixOf :: [Int] -> [Int] -> Bool)
transitivity :: (Show a, GenValid a) => (a -> a -> Bool) -> Property
transitivity func = transitivityOnGens func genValid shrinkValid

-- |
--
-- prop> transitivityOnArbitrary ((>) :: Int -> Int -> Bool)
-- prop> transitivityOnArbitrary ((>=) :: Int -> Int -> Bool)
-- prop> transitivityOnArbitrary ((==) :: Int -> Int -> Bool)
-- prop> transitivityOnArbitrary ((<=) :: Int -> Int -> Bool)
-- prop> transitivityOnArbitrary ((<) :: Int -> Int -> Bool)
-- prop> transitivityOnArbitrary (Data.List.isPrefixOf :: [Int] -> [Int] -> Bool)
-- prop> transitivityOnArbitrary (Data.List.isSuffixOf :: [Int] -> [Int] -> Bool)
-- prop> transitivityOnArbitrary (Data.List.isInfixOf :: [Int] -> [Int] -> Bool)
transitivityOnArbitrary :: (Show a, Arbitrary a) => (a -> a -> Bool) -> Property
transitivityOnArbitrary func = transitivityOnGens func arbitrary shrink
