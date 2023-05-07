{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.SydCheck.Relations.Reflexivity
  ( reflexivity,
    reflexivityOnGen,
    reflexiveOnElem,
  )
where

import SydCheck
import Test.Syd

reflexivity ::
  (Show a, GenValid a) =>
  (a -> a -> Bool) ->
  TypedProperty '[a]
reflexivity func = reflexivityOnGen func genValid

reflexivityOnGen ::
  Show a =>
  (a -> a -> Bool) ->
  Gen a ->
  TypedProperty '[a]
reflexivityOnGen func gen = forAll gen $ reflexiveOnElem func

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
  IO ()
reflexiveOnElem func a =
  func a a `shouldBe` True
