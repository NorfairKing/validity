{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Validity.Relations.Antisymmetry
  ( antisymmetricOnElemsWithEquality,
    antisymmetryOnGensWithEquality,
    antisymmetryOnGens,
    antisymmetry,
    antisymmetryOnArbitrary,
  )
where

import Data.GenValidity
import Test.QuickCheck
import Test.Validity.Property.Utils

-- |
--
-- \[
--   Antisymmetric(\prec, \doteq)
--   \quad\equiv\quad
--   \forall a, b: ((a \prec b) \wedge (b \prec a)) \Rightarrow (a \doteq b)
-- \]
antisymmetricOnElemsWithEquality ::
  -- | A relation
  (a -> a -> Bool) ->
  -- | An equivalence relation
  (a -> a -> Bool) ->
  a ->
  -- | Two elements
  a ->
  Bool
antisymmetricOnElemsWithEquality func eq a b =
  (func a b && func b a) ===> (a `eq` b)

antisymmetryOnGensWithEquality ::
  Show a =>
  (a -> a -> Bool) ->
  Gen (a, a) ->
  (a -> a -> Bool) ->
  (a -> [a]) ->
  Property
antisymmetryOnGensWithEquality func gen eq s =
  forAllShrink gen (shrinkT2 s) $
    uncurry $ antisymmetricOnElemsWithEquality func eq

antisymmetryOnGens ::
  (Show a, Eq a) =>
  (a -> a -> Bool) ->
  Gen (a, a) ->
  (a -> [a]) ->
  Property
antisymmetryOnGens func gen = antisymmetryOnGensWithEquality func gen (==)

-- |
--
-- prop> antisymmetry ((>) :: Int -> Int -> Bool)
-- prop> antisymmetry ((>=) :: Int -> Int -> Bool)
-- prop> antisymmetry ((<=) :: Int -> Int -> Bool)
-- prop> antisymmetry ((<) :: Int -> Int -> Bool)
-- prop> antisymmetry (Data.List.isPrefixOf :: [Int] -> [Int] -> Bool)
-- prop> antisymmetry (Data.List.isSuffixOf :: [Int] -> [Int] -> Bool)
-- prop> antisymmetry (Data.List.isInfixOf :: [Int] -> [Int] -> Bool)
-- prop> antisymmetry ((\x y -> even x && odd y) :: Int -> Int -> Bool)
antisymmetry :: (Show a, Eq a, GenValid a) => (a -> a -> Bool) -> Property
antisymmetry func = antisymmetryOnGens func genValid shrinkValid

-- |
--
-- prop> antisymmetryOnArbitrary ((>) :: Int -> Int -> Bool)
-- prop> antisymmetryOnArbitrary ((>=) :: Int -> Int -> Bool)
-- prop> antisymmetryOnArbitrary ((<=) :: Int -> Int -> Bool)
-- prop> antisymmetryOnArbitrary ((<) :: Int -> Int -> Bool)
-- prop> antisymmetryOnArbitrary (Data.List.isPrefixOf :: [Int] -> [Int] -> Bool)
-- prop> antisymmetryOnArbitrary (Data.List.isSuffixOf :: [Int] -> [Int] -> Bool)
-- prop> antisymmetryOnArbitrary (Data.List.isInfixOf :: [Int] -> [Int] -> Bool)
-- prop> antisymmetryOnArbitrary ((\x y -> even x && odd y) :: Int -> Int -> Bool)
antisymmetryOnArbitrary ::
  (Show a, Eq a, Arbitrary a) => (a -> a -> Bool) -> Property
antisymmetryOnArbitrary func = antisymmetryOnGens func arbitrary shrink
