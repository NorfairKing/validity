{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Validity.Relations.Reflexivity
    ( reflexivityOnGen
    , reflexivityOnValid
    , reflexivity
    , reflexivityOnArbitrary
    ) where

import           Data.GenValidity

import           Test.QuickCheck

reflexivityOnGen
    :: Show a
    => (a -> a -> Bool)
    -> Gen a
    -> Property
reflexivityOnGen func gen =
    forAll gen $ \a ->
        func a a

reflexivityOnValid
    :: (Show a, GenValidity a)
    => (a -> a -> Bool)
    -> Property
reflexivityOnValid func
    = reflexivityOnGen func genValid

reflexivity
    :: (Show a, GenValidity a)
    => (a -> a -> Bool)
    -> Property
reflexivity func
    = reflexivityOnGen func genUnchecked

reflexivityOnArbitrary
    :: (Show a, Arbitrary a)
    => (a -> a -> Bool)
    -> Property
reflexivityOnArbitrary func
    = reflexivityOnGen func arbitrary
