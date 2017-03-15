{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for GenValidity instances
module Test.Validity.GenValidity
    ( genValidGeneratesValid
    , genGeneratesValid
    , genInvalidGeneratesInvalid
    , genGeneratesInvalid
    ) where

import Data.Data

import Data.GenValidity

import Test.Hspec
import Test.QuickCheck

import Test.Validity.Utils

-- | @genValid@ only generates valid data
--
-- prop> genValidGeneratesValid @()
-- prop> genValidGeneratesValid @Bool
-- prop> genValidGeneratesValid @Ordering
-- prop> genValidGeneratesValid @Char
-- prop> genValidGeneratesValid @Int
-- prop> genValidGeneratesValid @Float
-- prop> genValidGeneratesValid @Double
-- prop> genValidGeneratesValid @Integer
-- prop> genValidGeneratesValid @(Maybe Int)
-- prop> genValidGeneratesValid @[Int]
genValidGeneratesValid
    :: forall a.
       (Show a, GenValid a)
    => Property
genValidGeneratesValid = genGeneratesValid @a genValid

-- | The given generator generates only valid data points
genGeneratesValid
    :: forall a.
       (Show a, Validity a)
    => Gen a -> Property
genGeneratesValid gen = forAll gen (`shouldSatisfy` isValid)

-- | @genValid@ only generates invalid data
--
-- prop> genInvalidGeneratesInvalid @Float
-- prop> genInvalidGeneratesInvalid @Double
-- prop> genInvalidGeneratesInvalid @(Maybe Double)
-- prop> genInvalidGeneratesInvalid @[Double]
genInvalidGeneratesInvalid
    :: forall a.
       (Show a, GenInvalid a)
    => Property
genInvalidGeneratesInvalid = genGeneratesInvalid @a genInvalid

-- | The given generator generates only invalid data points
genGeneratesInvalid
    :: forall a.
       (Show a, Validity a)
    => Gen a -> Property
genGeneratesInvalid gen = forAll gen (`shouldNotSatisfy` isValid)
