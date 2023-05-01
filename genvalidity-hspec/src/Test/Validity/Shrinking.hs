{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for Shrinking functions
--
-- You will need @TypeApplications@ to use these.
module Test.Validity.Shrinking
  ( shrinkValidSpec,
    shrinkValidSpecWithLimit,
    shrinkValidPreservesValidOnGenValid,
    shrinkValidPreservesValidOnGenValidWithLimit,
    shrinkPreservesValidOnGenValid,
    shrinkValidPreservesValid,
    shrinkingStaysValid,
    shrinkingPreserves,
    shrinkValidDoesNotShrinkToItself,
    shrinkValidDoesNotShrinkToItselfWithLimit,
  )
where

import Control.Monad
import Data.Data
import Data.GenValidity
import Test.Hspec
import Test.QuickCheck
import Test.Validity.Shrinking.Property
import Test.Validity.Utils

shrinkValidSpec ::
  forall a.
  (Show a, Eq a, Typeable a, GenValid a) =>
  Spec
shrinkValidSpec =
  describe ("shrinkValid :: " ++ nameOf @(a -> [a])) $ do
    it "preserves validity" $
      forAll (genValid @a) $
        \a -> forM_ (shrinkValid a) shouldBeValid
    it "never shrinks to itself for valid values" $
      shrinkValidDoesNotShrinkToItself @a

shrinkValidSpecWithLimit ::
  forall a.
  (Show a, Eq a, Typeable a, GenValid a) =>
  Int ->
  Spec
shrinkValidSpecWithLimit l =
  describe ("shrinkValid :: " ++ nameOf @(a -> [a])) $ do
    it (unwords ["preserves validity for the first", show l, "elements"]) $
      forAll (genValid @a) $ \a ->
        forM_ (take l $ shrinkValid a) shouldBeValid
    it
      ( unwords
          [ "never shrinks to itself for valid values for the first",
            show l,
            "elements"
          ]
      )
      $ shrinkValidDoesNotShrinkToItselfWithLimit @a l

shrinkValidPreservesValidOnGenValid ::
  forall a.
  (Show a, GenValid a) =>
  Property
shrinkValidPreservesValidOnGenValid =
  shrinkingStaysValid @a genValid shrinkValid

shrinkValidPreservesValidOnGenValidWithLimit ::
  forall a.
  (Show a, GenValid a) =>
  Int ->
  Property
shrinkValidPreservesValidOnGenValidWithLimit =
  shrinkingStaysValidWithLimit @a genValid shrinkValid

shrinkValidDoesNotShrinkToItself ::
  forall a.
  (Show a, Eq a, GenValid a) =>
  Property
shrinkValidDoesNotShrinkToItself =
  shrinkDoesNotShrinkToItself @a shrinkValid

shrinkValidDoesNotShrinkToItselfWithLimit ::
  forall a.
  (Show a, Eq a, GenValid a) =>
  Int ->
  Property
shrinkValidDoesNotShrinkToItselfWithLimit =
  shrinkDoesNotShrinkToItselfOnValidWithLimit @a shrinkValid
