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
    , shrinkInvalidSpec
    , shrinkValidPreservesValidOnGenValid
    , shrinkInvalidPreservesInvalidOnGenInvalid
    , shrinkPreservesValidOnGenValid
    , shrinkPreservesInvalidOnGenInvalid
    , shrinkValidPreservesValid
    , shrinkInvalidPreservesInvalid
    , shrinkingStaysValid
    , shrinkingStaysInvalid
    , shrinkingPreserves
    ) where

import Data.Data

import Data.GenValidity

import Test.Hspec
import Test.QuickCheck

import Test.Validity.Shrinking.Property
import Test.Validity.Utils

shrinkValiditySpec ::
       forall a. (Show a, Typeable a, GenValid a, GenInvalid a)
    => Spec
shrinkValiditySpec = do
    shrinkValidSpec @a
    shrinkInvalidSpec @a

shrinkValidSpec ::
       forall a. (Show a, Typeable a, GenValid a)
    => Spec
shrinkValidSpec =
    describe ("shrinkValid :: " ++ nameOf @(a -> [a])) $
    it "preserves validity" $ shrinkValidPreservesValidOnGenValid @a

shrinkInvalidSpec ::
       forall a. (Show a, Typeable a, GenInvalid a)
    => Spec
shrinkInvalidSpec =
    describe ("shrinkInvalid :: " ++ nameOf @(a -> [a])) $
    it "preserves invalidity" $ shrinkInvalidPreservesInvalidOnGenInvalid @a

shrinkValidPreservesValidOnGenValid ::
       forall a. (Show a, GenValid a)
    => Property
shrinkValidPreservesValidOnGenValid =
    shrinkingStaysValid @a genValid shrinkValid

shrinkInvalidPreservesInvalidOnGenInvalid ::
       forall a. (Show a, GenInvalid a)
    => Property
shrinkInvalidPreservesInvalidOnGenInvalid =
    shrinkingStaysInvalid @a genInvalid shrinkInvalid
