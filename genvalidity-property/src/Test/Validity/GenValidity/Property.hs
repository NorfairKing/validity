{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for GenValidity instances
module Test.Validity.GenValidity.Property
    ( genGeneratesValid
    , genGeneratesInvalid
    ) where

import Data.Data

import Data.GenValidity

import Test.Hspec
import Test.QuickCheck

import Test.Validity.Property.Utils

-- | The given generator generates only valid data points
genGeneratesValid
    :: forall a.
       (Show a, Validity a)
    => Gen a -> Property
genGeneratesValid gen = forAll gen (`shouldSatisfy` isValid)

-- | The given generator generates only invalid data points
genGeneratesInvalid
    :: forall a.
       (Show a, Validity a)
    => Gen a -> Property
genGeneratesInvalid gen = forAll gen (`shouldSatisfy` isInvalid)
