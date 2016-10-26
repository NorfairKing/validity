{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Validity.Relations.Antisymmetry
    ( antisymmetricOnElemsWithEquality
    , antisymmetryOnGensWithEquality
    , antisymmetryOnGens
    , antisymmetryOnValid
    , antisymmetry
    , antisymmetryOnArbitrary
    ) where

import           Data.GenValidity

import           Test.QuickCheck

import           Test.Validity.Utils

-- |
--
-- \[
--   Antisymmetric(\prec, \doteq)
--   \quad\equiv\quad
--   \forall a, b: ((a \prec b) \wedge (b \prec a)) \Rightarrow (a \doteq b)
-- \]
antisymmetricOnElemsWithEquality
    :: (a -> a -> Bool) -- ^ A relation
    -> (a -> a -> Bool) -- ^ An equivalence relation
    -> a -> a           -- ^ Two elements
    -> Bool
antisymmetricOnElemsWithEquality func eq a b =
    (func a b && func b a) ===> (a `eq` b)

antisymmetryOnGensWithEquality
    :: Show a
    => (a -> a -> Bool)
    -> Gen (a, a)
    -> (a -> a -> Bool)
    -> Property
antisymmetryOnGensWithEquality func gen eq =
    forAll gen $ uncurry $ antisymmetricOnElemsWithEquality func eq

antisymmetryOnGens
    :: (Show a, Eq a)
    => (a -> a -> Bool)
    -> Gen (a, a)
    -> Property
antisymmetryOnGens func gen
    = antisymmetryOnGensWithEquality func gen (==)

antisymmetryOnValid
    :: (Show a, Eq a, GenValidity a)
    => (a -> a -> Bool)
    -> Property
antisymmetryOnValid func =
    antisymmetryOnGens func genValid

antisymmetry
    :: (Show a, Eq a, GenValidity a)
    => (a -> a -> Bool)
    -> Property
antisymmetry func =
    antisymmetryOnGens func genUnchecked

-- |
--
-- prop> antisymmetryOnArbitrary ((<=) :: Int -> Int -> Bool)
antisymmetryOnArbitrary
    :: (Show a, Eq a, Arbitrary a)
    => (a -> a -> Bool)
    -> Property
antisymmetryOnArbitrary func =
    antisymmetryOnGens func arbitrary
