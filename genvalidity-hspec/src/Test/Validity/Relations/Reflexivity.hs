{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Validity.Relations.Reflexivity
    ( reflexiveOnElem
    , reflexivityOnGen
    , reflexivityOnValid
    , reflexivity
    , reflexivityOnArbitrary
    ) where

import Data.GenValidity

import Test.QuickCheck

-- |
--
-- \[
--   Reflexive(\prec)
--   \quad\equiv\quad
--   \forall a: (a \prec a)
-- \]
reflexiveOnElem
    :: (a -> a -> Bool) -- ^ A relation
    -> a -- ^ An element
    -> Bool
reflexiveOnElem func a = func a a

reflexivityOnGen
    :: Show a
    => (a -> a -> Bool) -> Gen a -> Property
reflexivityOnGen func gen = forAll gen $ reflexiveOnElem func

-- |
--
-- prop> reflexivityOnValid ((<=) @Double)
-- prop> reflexivityOnValid ((==) @Double)
-- prop> reflexivityOnValid ((>=) @Double)
reflexivityOnValid
    :: (Show a, GenValid a)
    => (a -> a -> Bool) -> Property
reflexivityOnValid func = reflexivityOnGen func genValid

-- |
--
-- prop> reflexivity ((<=) @Int)
-- prop> reflexivity ((==) @Int)
-- prop> reflexivity ((>=) @Int)
reflexivity
    :: (Show a, GenUnchecked a)
    => (a -> a -> Bool) -> Property
reflexivity func = reflexivityOnGen func genUnchecked

-- |
--
-- prop> reflexivityOnArbitrary ((<=) @Int)
-- prop> reflexivityOnArbitrary ((==) @Int)
-- prop> reflexivityOnArbitrary ((>=) @Int)
reflexivityOnArbitrary
    :: (Show a, Arbitrary a)
    => (a -> a -> Bool) -> Property
reflexivityOnArbitrary func = reflexivityOnGen func arbitrary
