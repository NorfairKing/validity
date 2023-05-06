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
  forAll gen $ uncurry $ symmetricOnElems func

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
symmetricOnElems func a b =
  func a b `shouldBe` func b a
