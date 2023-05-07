{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.SydCheck.Relations.Antireflexivity
  ( antireflexivity,
    antireflexivityOnGen,
    antireflexiveOnElem,
  )
where

import SydCheck
import Test.Syd

-- |
--
-- prop> antireflexivity ((<) :: Int -> Int -> Bool)
-- prop> antireflexivity ((/=) :: Int -> Int -> Bool)
-- prop> antireflexivity ((>) :: Int -> Int -> Bool)
antireflexivity ::
  (Show a, GenValid a) =>
  (a -> a -> Bool) ->
  TypedProperty '[a]
antireflexivity func = antireflexivityOnGen func genValid

antireflexivityOnGen ::
  Show a =>
  (a -> a -> Bool) ->
  Gen a ->
  TypedProperty '[a]
antireflexivityOnGen func gen =
  forAll gen $
    antireflexiveOnElem func

-- |
--
-- \[
--   Antireflexive(\prec)
--   \quad\equiv\quad
--   \forall a: \neg (a \prec a)
-- \]
antireflexiveOnElem ::
  Show a =>
  -- | A relation
  (a -> a -> Bool) ->
  -- | An element
  a ->
  IO ()
antireflexiveOnElem func a =
  context (unwords [ppShow a, "`rel`", ppShow a, ":", show (func a a)]) $
    func a a `shouldBe` True
