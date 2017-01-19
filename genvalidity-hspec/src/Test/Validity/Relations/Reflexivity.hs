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
-- prop> reflexivityOnValid (Data.List.isPrefixOf @Double)
-- prop> reflexivityOnValid (Data.List.isSuffixOf @Double)
-- prop> reflexivityOnValid (Data.List.isInfixOf @Double)
-- prop> reflexivityOnValid (Data.List.isSubsequenceOf @Double)
reflexivityOnValid
    :: (Show a, GenValid a)
    => (a -> a -> Bool) -> Property
reflexivityOnValid func = reflexivityOnGen func genValid

-- |
--
-- prop> reflexivity ((<=) @Int)
-- prop> reflexivity ((==) @Int)
-- prop> reflexivity ((>=) @Int)
-- prop> reflexivity (Data.List.isPrefixOf @Int)
-- prop> reflexivity (Data.List.isSuffixOf @Int)
-- prop> reflexivity (Data.List.isInfixOf @Int)
-- prop> reflexivity (Data.List.isSubsequenceOf @Int)
reflexivity
    :: (Show a, GenUnchecked a)
    => (a -> a -> Bool) -> Property
reflexivity func = reflexivityOnGen func genUnchecked

-- |
--
-- prop> reflexivityOnArbitrary ((<=) @Int)
-- prop> reflexivityOnArbitrary ((==) @Int)
-- prop> reflexivityOnArbitrary ((>=) @Int)
-- prop> reflexivityOnArbitrary (Data.List.isPrefixOf @Int)
-- prop> reflexivityOnArbitrary (Data.List.isSuffixOf @Int)
-- prop> reflexivityOnArbitrary (Data.List.isInfixOf @Int)
-- prop> reflexivityOnArbitrary (Data.List.isSubsequenceOf @Int)
reflexivityOnArbitrary
    :: (Show a, Arbitrary a)
    => (a -> a -> Bool) -> Property
reflexivityOnArbitrary func = reflexivityOnGen func arbitrary
