{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Validity.Relations.Transitivity
    ( transitivityOnGens
    , transitivityOnValid
    , transitivity
    , transitivityOnArbitrary
    ) where

import           Data.GenValidity

import           Test.QuickCheck

import           Test.Validity.Utils

transitivityOnGens
    :: Show a
    => (a -> a -> Bool)
    -> Gen (a, a, a)
    -> Property
transitivityOnGens func gen =
    forAll gen $ \(a, b, c) ->
        (func a b && func b c)
        ===> func a c

transitivityOnValid
    :: (Show a, GenValidity a)
    => (a -> a -> Bool)
    -> Property
transitivityOnValid func
    = transitivityOnGens func genValid

transitivity
    :: (Show a, GenValidity a)
    => (a -> a -> Bool)
    -> Property
transitivity func
    = transitivityOnGens func genUnchecked

transitivityOnArbitrary
    :: (Show a, Arbitrary a)
    => (a -> a -> Bool)
    -> Property
transitivityOnArbitrary func
    = transitivityOnGens func arbitrary

