{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Validity.Relations.Transitivity
    ( transitiveOnElems
    , transitivityOnGens
    , transitivityOnValid
    , transitivity
    , transitivityOnArbitrary
    ) where

import Data.GenValidity

import Test.QuickCheck

import Test.Validity.Utils

-- |
--
-- \[
--   Transitive(\prec)
--   \quad\equiv\quad
--   \forall a, b, c: ((a \prec b) \wedge (b \prec c)) \Rightarrow (a \prec c)
-- \]
transitiveOnElems
    :: (a -> a -> Bool) -- ^ A relation
    -> a
    -> a
    -> a -- ^ Three elements
    -> Bool
transitiveOnElems func a b c = (func a b && func b c) ===> func a c

transitivityOnGens
    :: Show a
    => (a -> a -> Bool) -> Gen (a, a, a) -> Property
transitivityOnGens func gen =
    forAll gen $ \(a, b, c) -> transitiveOnElems func a b c

-- |
--
-- prop> transitivityOnValid ((>) @Double)
-- prop> transitivityOnValid ((>=) @Double)
-- prop> transitivityOnValid ((==) @Double)
-- prop> transitivityOnValid ((<=) @Double)
-- prop> transitivityOnValid ((<) @Double)
transitivityOnValid
    :: (Show a, GenValid a)
    => (a -> a -> Bool) -> Property
transitivityOnValid func = transitivityOnGens func genValid

-- |
--
-- prop> transitivity ((>) @Int)
-- prop> transitivity ((>=) @Int)
-- prop> transitivity ((==) @Int)
-- prop> transitivity ((<=) @Int)
-- prop> transitivity ((<) @Int)
transitivity
    :: (Show a, GenUnchecked a)
    => (a -> a -> Bool) -> Property
transitivity func = transitivityOnGens func genUnchecked

-- |
--
-- prop> transitivityOnArbitrary ((>) @Int)
-- prop> transitivityOnArbitrary ((>=) @Int)
-- prop> transitivityOnArbitrary ((==) @Int)
-- prop> transitivityOnArbitrary ((<=) @Int)
-- prop> transitivityOnArbitrary ((<) @Int)
transitivityOnArbitrary
    :: (Show a, Arbitrary a)
    => (a -> a -> Bool) -> Property
transitivityOnArbitrary func = transitivityOnGens func arbitrary
