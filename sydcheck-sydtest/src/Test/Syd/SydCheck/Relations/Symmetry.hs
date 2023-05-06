{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.SydCheck.Relations.Symmetry
  ( symmetry,
    symmetryOnGens,
    symmetricOnElems,
  )
where

import SydCheck
import Test.Syd
import Test.Syd.SydCheck.Utils

-- |
--
-- prop> symmetry ((==) :: Int -> Int -> Bool)
-- prop> symmetry ((/=) :: Int -> Int -> Bool)
symmetry ::
  (Show a, GenValid a) =>
  (a -> a -> Bool) ->
  TypedPropertyT '[(a, a)] IO
symmetry func = symmetryOnGens func genValid

symmetryOnGens ::
  Show a =>
  (a -> a -> Bool) ->
  Gen (a, a) ->
  TypedPropertyT '[(a, a)] IO
symmetryOnGens func gen =
  forAll gen $ \(a, b) ->
    symmetricOnElems func a b

-- |
--
-- \[
--   Symmetric(\prec)
--   \quad\equiv\quad
--   \forall a, b: (a \prec b) \Leftrightarrow (b \prec a)
-- \]
symmetricOnElems ::
  -- | A relation
  (a -> a -> Bool) ->
  a ->
  -- | Two elements
  a ->
  IO ()
symmetricOnElems func a b = do
  let oneWay = func a b
  let otherWay = func b a
  let ctx =
        unlines
          [ unwords ["a `rel` b:", show oneWay],
            unwords ["b `rel` a:", show otherWay]
          ]
  context ctx $ oneWay `shouldBe` otherWay
