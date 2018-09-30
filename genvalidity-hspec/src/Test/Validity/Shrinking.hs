{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for Shrinking functions
--
-- You will need @TypeApplications@ to use these.
module Test.Validity.Shrinking
    ( shrinkValiditySpec
    , shrinkValidSpec
    , shrinkValidSpecWithLimit
    , shrinkInvalidSpec
    , shrinkValidPreservesValidOnGenValid
    , shrinkValidPreservesValidOnGenValidWithLimit
    , shrinkInvalidPreservesInvalidOnGenInvalid
    , shrinkPreservesValidOnGenValid
    , shrinkPreservesInvalidOnGenInvalid
    , shrinkValidPreservesValid
    , shrinkInvalidPreservesInvalid
    , shrinkingStaysValid
    , shrinkingStaysInvalid
    , shrinkingPreserves
    , shrinkUncheckedDoesNotShrinkToItself
    , shrinkUncheckedDoesNotShrinkToItselfWithLimit
    , shrinkValidDoesNotShrinkToItself
    , shrinkValidDoesNotShrinkToItselfWithLimit
    , shrinkInvalidDoesNotShrinkToItself
    , shrinkInvalidDoesNotShrinkToItselfWithLimit
    ) where

import Data.Data

import Data.GenValidity

import Control.Monad

import Test.Hspec
import Test.QuickCheck

import Test.Validity.Shrinking.Property
import Test.Validity.Utils

shrinkValiditySpec ::
       forall a. (Show a, Eq a, Typeable a, GenValid a, GenInvalid a)
    => Spec
shrinkValiditySpec = do
    shrinkValidSpec @a
    shrinkInvalidSpec @a

shrinkValidSpec ::
       forall a. (Show a, Eq a, Typeable a, GenValid a)
    => Spec
shrinkValidSpec =
    describe ("shrinkValid :: " ++ nameOf @(a -> [a])) $ do
        it "preserves validity" $
            forAll (genValid @a) $ \a -> forM_ (shrinkValid a) shouldBeValid
        it "never shrinks to itself for valid values" $
            shrinkValidDoesNotShrinkToItself @a

shrinkValidSpecWithLimit ::
       forall a. (Show a, Eq a, Typeable a, GenValid a)
    => Int
    -> Spec
shrinkValidSpecWithLimit l =
    describe ("shrinkValid :: " ++ nameOf @(a -> [a])) $ do
        it (unwords ["preserves validity for the first", show l, "elements"]) $
            forAll (genValid @a) $ \a -> forM_ (take l $ shrinkValid a) shouldBeValid
        it
            (unwords
                 [ "never shrinks to itself for valid values for the first"
                 , show l
                 , "elements"
                 ]) $
            shrinkValidDoesNotShrinkToItselfWithLimit @a l

shrinkInvalidSpec ::
       forall a. (Show a, Typeable a, GenInvalid a)
    => Spec
shrinkInvalidSpec =
    describe ("shrinkInvalid :: " ++ nameOf @(a -> [a])) $ do
        it "preserves invalidity" $
            forAll (genInvalid @a) $ \a ->
                forM_ (shrinkInvalid a) shouldBeInvalid

shrinkValidPreservesValidOnGenValid ::
       forall a. (Show a, GenValid a)
    => Property
shrinkValidPreservesValidOnGenValid =
    shrinkingStaysValid @a genValid shrinkValid

shrinkValidPreservesValidOnGenValidWithLimit ::
       forall a. (Show a, GenValid a)
    => Int
    -> Property
shrinkValidPreservesValidOnGenValidWithLimit =
    shrinkingStaysValidWithLimit @a genValid shrinkValid

shrinkInvalidPreservesInvalidOnGenInvalid ::
       forall a. (Show a, GenInvalid a)
    => Property
shrinkInvalidPreservesInvalidOnGenInvalid =
    shrinkingStaysInvalid @a genInvalid shrinkInvalid

shrinkUncheckedDoesNotShrinkToItself ::
       forall a. (Show a, Eq a, GenUnchecked a)
    => Property
shrinkUncheckedDoesNotShrinkToItself =
    shrinkDoesNotShrinkToItself @a shrinkUnchecked

shrinkValidDoesNotShrinkToItself ::
       forall a. (Show a, Eq a, GenValid a)
    => Property
shrinkValidDoesNotShrinkToItself = shrinkDoesNotShrinkToItself @a shrinkValid

shrinkInvalidDoesNotShrinkToItself ::
       forall a. (Show a, Eq a, GenInvalid a)
    => Property
shrinkInvalidDoesNotShrinkToItself =
    shrinkDoesNotShrinkToItself @a shrinkInvalid

shrinkInvalidDoesNotShrinkToItselfWithLimit ::
       forall a. (Show a, Eq a, GenInvalid a)
    => Int
    -> Property
shrinkInvalidDoesNotShrinkToItselfWithLimit =
    shrinkDoesNotShrinkToItselfWithLimit @a shrinkInvalid

shrinkValidDoesNotShrinkToItselfWithLimit ::
       forall a. (Show a, Eq a, GenValid a)
    => Int
    -> Property
shrinkValidDoesNotShrinkToItselfWithLimit =
    shrinkDoesNotShrinkToItselfWithLimit @a shrinkValid

shrinkUncheckedDoesNotShrinkToItselfWithLimit ::
       forall a. (Show a, Eq a, GenUnchecked a)
    => Int
    -> Property
shrinkUncheckedDoesNotShrinkToItselfWithLimit =
    shrinkDoesNotShrinkToItselfWithLimit @a shrinkUnchecked
