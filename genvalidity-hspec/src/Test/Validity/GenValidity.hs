{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for GenValidity instances
module Test.Validity.GenValidity
    ( genValiditySpec
    , genValidSpec
    , genInvalidSpec
    , genValidGeneratesValid
    , genGeneratesValid
    , genInvalidGeneratesInvalid
    , genGeneratesInvalid
    ) where

import Data.Data

import Data.GenValidity

import Test.Hspec
import Test.QuickCheck

import Test.Validity.Utils

-- | A spec for properties of 'GenValid' and 'GenInvalid' instances.
--
-- In general it is a good idea to add this spec to your test suite if you
-- write a custom implementation of @genValid@ or @genInvalid@.
--
-- Example usage:
--
-- > genValiditySpec @Int
genValiditySpec
    :: forall a.
       (Typeable a, Show a, GenValid a, GenInvalid a)
    => Spec
genValiditySpec = do
    genValidSpec @a
    genInvalidSpec @a

-- | A @Spec@ that specifies that @genValid@ only generates valid data.
--
-- Example usage:
--
-- > genValidSpec @Int
genValidSpec
    :: forall a.
       (Typeable a, Show a, GenValid a)
    => Spec
genValidSpec =
    parallel $ do
        let name = nameOf @a
        describe ("GenValid " ++ name) $ do
            describe ("genValid   :: Gen " ++ name) $
                it ("only generates valid \'" ++ name ++ "\'s") $
                genValidGeneratesValid @a

-- | A @Spec@ that specifies that @genInvalid@ only generates invalid data.
--
-- Example usage:
--
-- > genInvalidSpec @Double
genInvalidSpec
    :: forall a.
       (Typeable a, Show a, GenInvalid a)
    => Spec
genInvalidSpec =
    parallel $ do
        let name = nameOf @a
        describe ("GenInvalid " ++ name) $ do
            describe ("genInvalid :: Gen " ++ name) $
                it ("only generates invalid \'" ++ name ++ "\'s") $
                genInvalidGeneratesInvalid @a

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
