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
-- prop> reflexivityOnValid ((<=) :: Double -> Double -> Bool)
-- prop> reflexivityOnValid ((==) :: Double -> Double -> Bool)
-- prop> reflexivityOnValid ((>=) :: Double -> Double -> Bool)
-- prop> reflexivityOnValid (Data.List.isPrefixOf :: [Double] -> [Double] -> Bool)
-- prop> reflexivityOnValid (Data.List.isSuffixOf :: [Double] -> [Double] -> Bool)
-- prop> reflexivityOnValid (Data.List.isInfixOf :: [Double] -> [Double] -> Bool)
reflexivityOnValid
    :: (Show a, GenValid a)
    => (a -> a -> Bool) -> Property
reflexivityOnValid func = reflexivityOnGen func genValid

-- |
--
-- prop> reflexivity ((<=) :: Int -> Int -> Bool)
-- prop> reflexivity ((==) :: Int -> Int -> Bool)
-- prop> reflexivity ((>=) :: Int -> Int -> Bool)
-- prop> reflexivity (Data.List.isPrefixOf :: [Int] -> [Int] -> Bool)
-- prop> reflexivity (Data.List.isSuffixOf :: [Int] -> [Int] -> Bool)
-- prop> reflexivity (Data.List.isInfixOf :: [Int] -> [Int] -> Bool)
reflexivity
    :: (Show a, GenUnchecked a)
    => (a -> a -> Bool) -> Property
reflexivity func = reflexivityOnGen func genUnchecked

-- |
--
-- prop> reflexivityOnArbitrary ((<=) :: Int -> Int -> Bool)
-- prop> reflexivityOnArbitrary ((==) :: Int -> Int -> Bool)
-- prop> reflexivityOnArbitrary ((>=) :: Int -> Int -> Bool)
-- prop> reflexivityOnArbitrary (Data.List.isPrefixOf :: [Int] -> [Int] -> Bool)
-- prop> reflexivityOnArbitrary (Data.List.isSuffixOf :: [Int] -> [Int] -> Bool)
-- prop> reflexivityOnArbitrary (Data.List.isInfixOf :: [Int] -> [Int] -> Bool)
reflexivityOnArbitrary
    :: (Show a, Arbitrary a)
    => (a -> a -> Bool) -> Property
reflexivityOnArbitrary func = reflexivityOnGen func arbitrary
