{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Validity.Relations.Antisymmetry
    ( antisymmetryOnGensWithEquality
    , antisymmetryOnGensEq
    , antisymmetryOnValid
    , antisymmetry
    , antisymmetryOnArbitrary
    ) where

import           Data.GenValidity

import           Test.QuickCheck

import           Test.Validity.Utils

antisymmetryOnGensWithEquality
    :: Show a
    => (a -> a -> Bool)
    -> Gen (a, a)
    -> (a -> a -> Bool)
    -> Property
antisymmetryOnGensWithEquality func gen eq =
    forAll gen $ \(a, b) ->
        (func a b && func b a)
        ===> (a `eq` b)

antisymmetryOnGensEq
    :: (Show a, Eq a)
    => (a -> a -> Bool)
    -> Gen (a, a)
    -> Property
antisymmetryOnGensEq func gen
    = antisymmetryOnGensWithEquality func gen (==)

antisymmetryOnValid
    :: (Show a, Eq a, GenValidity a)
    => (a -> a -> Bool)
    -> Property
antisymmetryOnValid func =
    antisymmetryOnGensEq func genValid

antisymmetry
    :: (Show a, Eq a, GenValidity a)
    => (a -> a -> Bool)
    -> Property
antisymmetry func =
    antisymmetryOnGensEq func genUnchecked

antisymmetryOnArbitrary
    :: (Show a, Eq a, Arbitrary a)
    => (a -> a -> Bool)
    -> Property
antisymmetryOnArbitrary func =
    antisymmetryOnGensEq func arbitrary
