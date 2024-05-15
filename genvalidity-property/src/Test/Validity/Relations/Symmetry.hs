{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Validity.Relations.Symmetry
  ( symmetricOnElems,
    symmetryOnGens,
    symmetry,
    symmetryOnArbitrary,
  )
where

import Data.GenValidity
import Test.QuickCheck
import Test.Validity.Property.Utils

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
  Bool
symmetricOnElems func a b = func a b <==> func b a

symmetryOnGens ::
  (Show a) => (a -> a -> Bool) -> Gen (a, a) -> (a -> [a]) -> Property
symmetryOnGens func gen s =
  forAllShrink gen (shrinkT2 s) $ uncurry $ symmetricOnElems func

-- |
--
-- prop> symmetry ((==) :: Int -> Int -> Bool)
-- prop> symmetry ((/=) :: Int -> Int -> Bool)
symmetry :: (Show a, GenValid a) => (a -> a -> Bool) -> Property
symmetry func = symmetryOnGens func genValid shrinkValid

-- |
--
-- prop> symmetryOnArbitrary ((==) :: Int -> Int -> Bool)
-- prop> symmetryOnArbitrary ((/=) :: Int -> Int -> Bool)
symmetryOnArbitrary :: (Show a, Arbitrary a) => (a -> a -> Bool) -> Property
symmetryOnArbitrary func = symmetryOnGens func arbitrary shrink
