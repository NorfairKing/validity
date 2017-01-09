{-# LANGUAGE MultiParamTypeClasses #-}
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

-- | A @Spec@ that specifies that @genValid@ only generates valid data and that
-- @genInvalid@ only generates invalid data.
--
-- In general it is a good idea to add this spec to your test suite if you
-- write a custom implementation of @genValid@ or @genInvalid@.
--
-- Example usage:
--
-- > genValiditySpec (Proxy :: Proxy MyData)
genValiditySpec
    :: (Typeable a, Show a, GenValid a, GenInvalid a)
    => Proxy a -> Spec
genValiditySpec proxy = do
    genValidSpec proxy
    genInvalidSpec proxy

genValidSpec
    :: (Typeable a, Show a, GenValid a)
    => Proxy a -> Spec
genValidSpec proxy =
    parallel $ do
        let name = nameOf proxy
        describe ("GenValid " ++ name) $ do
            describe ("genValid   :: Gen " ++ name) $
                it ("only generates valid \'" ++ name ++ "\'s") $
                genValidGeneratesValid proxy

genInvalidSpec
    :: (Typeable a, Show a, GenValid a, GenInvalid a)
    => Proxy a -> Spec
genInvalidSpec proxy =
    parallel $ do
        let name = nameOf proxy
        describe ("GenInvalid " ++ name) $ do
            describe ("genInvalid :: Gen " ++ name) $
                it ("only generates invalid \'" ++ name ++ "\'s") $
                genInvalidGeneratesInvalid proxy

-- | @genValid@ only generates valid data
--
-- prop> genValidGeneratesValid (Proxy :: Proxy ())
-- prop> genValidGeneratesValid (Proxy :: Proxy Bool)
-- prop> genValidGeneratesValid (Proxy :: Proxy Ordering)
-- prop> genValidGeneratesValid (Proxy :: Proxy Char)
-- prop> genValidGeneratesValid (Proxy :: Proxy Int)
-- prop> genValidGeneratesValid (Proxy :: Proxy Float)
-- prop> genValidGeneratesValid (Proxy :: Proxy Double)
-- prop> genValidGeneratesValid (Proxy :: Proxy Integer)
-- prop> genValidGeneratesValid (Proxy :: Proxy [Int])
genValidGeneratesValid
    :: forall a.
       (Show a, GenValid a)
    => Proxy a -> Property
genValidGeneratesValid _ = genGeneratesValid (genValid :: Gen a)

-- | The given generator generates only valid data points
genGeneratesValid
    :: (Show a, Validity a)
    => Gen a -> Property
genGeneratesValid gen = forAll gen (`shouldSatisfy` isValid)

-- | @genValid@ only generates invalid data
--
-- prop> genInvalidGeneratesInvalid (Proxy :: Proxy Float)
-- prop> genInvalidGeneratesInvalid (Proxy :: Proxy Double)
-- prop> genInvalidGeneratesInvalid (Proxy :: Proxy [Double])
genInvalidGeneratesInvalid
    :: forall a.
       (Show a, GenInvalid a)
    => Proxy a -> Property
genInvalidGeneratesInvalid _ = genGeneratesInvalid (genInvalid :: Gen a)

-- | The given generator generates only invalid data points
genGeneratesInvalid
    :: (Show a, Validity a)
    => Gen a -> Property
genGeneratesInvalid gen = forAll gen (`shouldNotSatisfy` isValid)
