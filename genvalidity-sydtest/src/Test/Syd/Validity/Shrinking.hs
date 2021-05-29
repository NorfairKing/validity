{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for Shrinking functions
--
-- You will need @TypeApplications@ to use these.
module Test.Syd.Validity.Shrinking
  ( shrinkValiditySpec,
    shrinkValidSpec,
    shrinkValidSpecWithLimit,
    shrinkInvalidSpec,
    shrinkValidPreservesValidOnGenValid,
    shrinkValidPreservesValidOnGenValidWithLimit,
    shrinkInvalidPreservesInvalidOnGenInvalid,
    shrinkPreservesValidOnGenValid,
    shrinkPreservesInvalidOnGenInvalid,
    shrinkValidPreservesValid,
    shrinkInvalidPreservesInvalid,
    shrinkingStaysValid,
    shrinkingStaysInvalid,
    shrinkingPreserves,
    shrinkUncheckedDoesNotShrinkToItself,
    shrinkUncheckedDoesNotShrinkToItselfWithLimit,
    shrinkValidDoesNotShrinkToItself,
    shrinkValidDoesNotShrinkToItselfWithLimit,
    shrinkInvalidDoesNotShrinkToItself,
    shrinkInvalidDoesNotShrinkToItselfWithLimit,
  )
where

import Control.Monad
import Data.Data
import Data.GenValidity
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity.Shrinking.Property
import Test.Syd.Validity.Utils

shrinkValiditySpec ::
  forall a.
  (Show a, Eq a, Typeable a, GenValid a, GenInvalid a) =>
  Spec
shrinkValiditySpec = do
  shrinkValidSpec @a
  shrinkInvalidSpec @a

shrinkValidSpec ::
  forall a.
  (Show a, Eq a, Typeable a, GenValid a) =>
  Spec
shrinkValidSpec =
  describe ("shrinkValid :: " ++ nameOf @(a -> [a])) $ do
    it "preserves validity" $
      forAll (genValid @a) $ \a -> forM_ (shrinkValid a) shouldBeValid
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

shrinkInvalidSpec ::
  forall a.
  (Show a, Typeable a, GenInvalid a) =>
  Spec
shrinkInvalidSpec =
  describe ("shrinkInvalid :: " ++ nameOf @(a -> [a])) $ do
    it "preserves invalidity" $
      forAll (genInvalid @a) $ \a ->
        forM_ (shrinkInvalid a) shouldBeInvalid

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

shrinkInvalidPreservesInvalidOnGenInvalid ::
  forall a.
  (Show a, GenInvalid a) =>
  Property
shrinkInvalidPreservesInvalidOnGenInvalid =
  shrinkingStaysInvalid @a genInvalid shrinkInvalid

shrinkUncheckedDoesNotShrinkToItself ::
  forall a.
  (Show a, Eq a, GenUnchecked a) =>
  Property
shrinkUncheckedDoesNotShrinkToItself =
  shrinkDoesNotShrinkToItself @a shrinkUnchecked

shrinkValidDoesNotShrinkToItself ::
  forall a.
  (Show a, Eq a, GenValid a) =>
  Property
shrinkValidDoesNotShrinkToItself =
  shrinkDoesNotShrinkToItselfOnValid @a shrinkValid

shrinkInvalidDoesNotShrinkToItself ::
  forall a.
  (Show a, Eq a, GenInvalid a) =>
  Property
shrinkInvalidDoesNotShrinkToItself =
  shrinkDoesNotShrinkToItselfOnInvalid @a shrinkInvalid

shrinkInvalidDoesNotShrinkToItselfWithLimit ::
  forall a.
  (Show a, Eq a, GenInvalid a) =>
  Int ->
  Property
shrinkInvalidDoesNotShrinkToItselfWithLimit =
  shrinkDoesNotShrinkToItselfOnInvalidWithLimit @a shrinkInvalid

shrinkValidDoesNotShrinkToItselfWithLimit ::
  forall a.
  (Show a, Eq a, GenValid a) =>
  Int ->
  Property
shrinkValidDoesNotShrinkToItselfWithLimit =
  shrinkDoesNotShrinkToItselfOnValidWithLimit @a shrinkValid

shrinkUncheckedDoesNotShrinkToItselfWithLimit ::
  forall a.
  (Show a, Eq a, GenUnchecked a) =>
  Int ->
  Property
shrinkUncheckedDoesNotShrinkToItselfWithLimit =
  shrinkDoesNotShrinkToItselfWithLimit @a shrinkUnchecked
