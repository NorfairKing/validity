{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Validity.Relations.Symmetry
    ( symmetryOnGens
    , symmetryOnValid
    , symmetry
    , symmetryOnArbitrary
    ) where

import           Data.GenValidity

import           Test.QuickCheck

import           Test.Validity.Utils

symmetryOnGens
    :: Show a
    => (a -> a -> Bool)
    -> Gen (a, a)
    -> Property
symmetryOnGens func gen =
    forAll gen $ \(a, b) ->
        (func a b <==> func b a)

symmetryOnValid
    :: (Show a, GenValidity a)
    => (a -> a -> Bool)
    -> Property
symmetryOnValid func =
    symmetryOnGens func genValid

symmetry
    :: (Show a, GenValidity a)
    => (a -> a -> Bool)
    -> Property
symmetry func =
    symmetryOnGens func genUnchecked

symmetryOnArbitrary
    :: (Show a, Arbitrary a)
    => (a -> a -> Bool)
    -> Property
symmetryOnArbitrary func =
    symmetryOnGens func arbitrary
