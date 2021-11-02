{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Validity.Relations.Reflexivity
  ( reflexiveOnElem,
    reflexivityOnGen,
    reflexivity,
    reflexivityOnArbitrary,
  )
where

import Data.GenValidity
import Test.QuickCheck

-- |
--
-- \[
--   Reflexive(\prec)
--   \quad\equiv\quad
--   \forall a: (a \prec a)
-- \]
reflexiveOnElem ::
  -- | A relation
  (a -> a -> Bool) ->
  -- | An element
  a ->
  Bool
reflexiveOnElem func a = func a a

reflexivityOnGen ::
  Show a => (a -> a -> Bool) -> Gen a -> (a -> [a]) -> Property
reflexivityOnGen func gen s = forAllShrink gen s $ reflexiveOnElem func

-- |
--
-- prop> reflexivity ((<=) :: Int -> Int -> Bool)
-- prop> reflexivity ((==) :: Int -> Int -> Bool)
-- prop> reflexivity ((>=) :: Int -> Int -> Bool)
-- prop> reflexivity (Data.List.isPrefixOf :: [Int] -> [Int] -> Bool)
-- prop> reflexivity (Data.List.isSuffixOf :: [Int] -> [Int] -> Bool)
-- prop> reflexivity (Data.List.isInfixOf :: [Int] -> [Int] -> Bool)
reflexivity :: (Show a, GenValid a) => (a -> a -> Bool) -> Property
reflexivity func = reflexivityOnGen func genValid shrinkValid

-- |
--
-- prop> reflexivityOnArbitrary ((<=) :: Int -> Int -> Bool)
-- prop> reflexivityOnArbitrary ((==) :: Int -> Int -> Bool)
-- prop> reflexivityOnArbitrary ((>=) :: Int -> Int -> Bool)
-- prop> reflexivityOnArbitrary (Data.List.isPrefixOf :: [Int] -> [Int] -> Bool)
-- prop> reflexivityOnArbitrary (Data.List.isSuffixOf :: [Int] -> [Int] -> Bool)
-- prop> reflexivityOnArbitrary (Data.List.isInfixOf :: [Int] -> [Int] -> Bool)
reflexivityOnArbitrary :: (Show a, Arbitrary a) => (a -> a -> Bool) -> Property
reflexivityOnArbitrary func = reflexivityOnGen func arbitrary shrink
