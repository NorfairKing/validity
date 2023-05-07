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

-- |
--
-- prop> symmetry ((==) :: Int -> Int -> Bool)
-- prop> symmetry ((/=) :: Int -> Int -> Bool)
symmetry ::
  (Show a, GenValid a) =>
  (a -> a -> Bool) ->
  TypedProperty '[(a, a)]
symmetry func = symmetryOnGens func genValid

symmetryOnGens ::
  Show a =>
  (a -> a -> Bool) ->
  Gen (a, a) ->
  TypedProperty '[(a, a)]
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
  Show a =>
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
          [ unwords [ppShow a, "`rel`", ppShow b, ":", show oneWay],
            unwords [ppShow b, "`rel`", ppShow a, ":", show otherWay]
          ]
  context ctx $ oneWay `shouldBe` otherWay
