{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Validity.GenValidity
    ( -- * Tests for GenValidity instances
      genValiditySpec
    , genValidityValidGeneratesValid
    , genGeneratesValid
    , genValidityInvalidGeneratesInvalid
    , genGeneratesInvalid
    ) where

import           Data.Data

import           Data.GenValidity

import           Test.Hspec
import           Test.QuickCheck

import           Test.Validity.Utils

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
    :: (Typeable a, Show a, GenValidity a)
    => Proxy a
    -> Spec
genValiditySpec proxy = do
    let name = nameOf proxy
    describe ("GenValidity " ++ name) $ do
        describe ("genValid   :: Gen " ++ name) $
            it ("only generates valid \'" ++ name ++ "\'s") $
                genValidityValidGeneratesValid proxy

        describe ("genInvalid :: Gen " ++ name) $
            it ("only generates invalid \'" ++ name ++ "\'s") $
                genValidityInvalidGeneratesInvalid proxy

-- | @genValid@ only generates valid data
genValidityValidGeneratesValid
    :: forall a. (Show a, GenValidity a)
    => Proxy a
    -> Property
genValidityValidGeneratesValid _ =
    genGeneratesValid (genValid :: Gen a)

-- | The given generator generates only valid data points
genGeneratesValid
    :: (Show a, Validity a)
    => Gen a
    -> Property
genGeneratesValid gen =
    forAll gen (`shouldSatisfy` isValid)


-- | @genValid@ only generates invalid data
genValidityInvalidGeneratesInvalid
    :: forall a. (Show a, GenValidity a)
    => Proxy a
    -> Property
genValidityInvalidGeneratesInvalid _ =
    genGeneratesInvalid (genInvalid :: Gen a)

-- | The given generator generates only invalid data points
genGeneratesInvalid
    :: (Show a, Validity a)
    => Gen a
    -> Property
genGeneratesInvalid gen =
    forAll gen (`shouldNotSatisfy` isValid)

