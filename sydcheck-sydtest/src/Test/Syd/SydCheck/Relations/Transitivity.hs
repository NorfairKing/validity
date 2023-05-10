{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.SydCheck.Relations.Transitivity
  ( transitivity,
    transitivityOnGens,
    transitiveOnElems,
  )
where

import Control.Monad
import Test.Syd
import Test.SydCheck

transitivity ::
  (Show a, GenValid a) =>
  (a -> a -> Bool) ->
  TypedProperty '[(a, a, a)]
transitivity func =
  transitivityOnGens func genValid

transitivityOnGens ::
  Show a => (a -> a -> Bool) -> Gen (a, a, a) -> TypedProperty '[(a, a, a)]
transitivityOnGens func gen =
  forAll gen $ \(a, b, c) ->
    transitiveOnElems func a b c

-- |
--
-- \[
--   Transitive(\prec)
--   \quad\equiv\quad
--   \forall a, b, c: ((a \prec b) \wedge (b \prec c)) \Rightarrow (a \prec c)
-- \]
transitiveOnElems ::
  Show a =>
  -- | A relation
  (a -> a -> Bool) ->
  -- | Three elements
  a ->
  a ->
  a ->
  IO ()
transitiveOnElems func a b c = do
  let resultAB = func a b
  let resultBC = func b c
  let resultAC = func a c
  let ctx =
        unlines
          [ unwords [ppShow a, "`rel`", ppShow b, ":", show resultAB],
            unwords [ppShow b, "`rel`", ppShow c, ":", show resultBC],
            unwords [ppShow a, "`rel`", ppShow c, ":", show resultAC]
          ]
  context ctx $
    when (resultAB && resultBC) $
      resultAC `shouldBe` True
