{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.SydCheck.Relations.Antisymmetry
  ( antisymmetryOnGens,
    antisymmetry,
    antisymmetryOnGensWithEquality,
    antisymmetricOnElemsWithEquality,
  )
where

import Control.Monad
import Test.Syd
import Test.SydCheck

-- |
--
-- prop> antisymmetry ((>) :: Int -> Int -> Bool)
-- prop> antisymmetry ((>=) :: Int -> Int -> Bool)
-- prop> antisymmetry ((<=) :: Int -> Int -> Bool)
-- prop> antisymmetry ((<) :: Int -> Int -> Bool)
-- prop> antisymmetry (Data.List.isPrefixOf :: [Int] -> [Int] -> Bool)
-- prop> antisymmetry (Data.List.isSuffixOf :: [Int] -> [Int] -> Bool)
-- prop> antisymmetry (Data.List.isInfixOf :: [Int] -> [Int] -> Bool)
-- prop> antisymmetry ((\x y -> even x && odd y) :: Int -> Int -> Bool)
antisymmetry :: (Show a, Eq a, GenValid a) => (a -> a -> Bool) -> TypedProperty '[(a, a)]
antisymmetry func = antisymmetryOnGens func genValid

antisymmetryOnGens ::
  (Show a, Eq a) =>
  (a -> a -> Bool) ->
  Gen (a, a) ->
  TypedProperty '[(a, a)]
antisymmetryOnGens func gen = antisymmetryOnGensWithEquality func gen (==)

antisymmetryOnGensWithEquality ::
  Show a =>
  (a -> a -> Bool) ->
  Gen (a, a) ->
  (a -> a -> Bool) ->
  TypedProperty '[(a, a)]
antisymmetryOnGensWithEquality func gen eq =
  forAll gen $ \(a, b) ->
    antisymmetricOnElemsWithEquality func eq a b

-- |
--
-- \[
--   Antisymmetric(\prec, \doteq)
--   \quad\equiv\quad
--   \forall a, b: ((a \prec b) \wedge (b \prec a)) \Rightarrow (a \doteq b)
-- \]
antisymmetricOnElemsWithEquality ::
  Show a =>
  -- | A relation
  (a -> a -> Bool) ->
  -- | An equivalence relation
  (a -> a -> Bool) ->
  -- | Two elements
  a ->
  a ->
  IO ()
antisymmetricOnElemsWithEquality func eq a b = do
  let resultAB = func a b
  let resultBA = func b a
  let equal = a `eq` b
  let ctx =
        unlines
          [ unwords [ppShow a, "`rel`", ppShow b, ":", show resultAB],
            unwords [ppShow b, "`rel`", ppShow a, ":", show resultBA],
            unwords [ppShow a, "`eq`", ppShow b, ":", show equal]
          ]
  when (resultAB && resultBA) $
    equal `shouldBe` True
