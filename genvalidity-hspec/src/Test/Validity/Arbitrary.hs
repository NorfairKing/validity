{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for Arbitrary instances involving Validity
--
-- You will need @TypeApplications@ to use these.
module Test.Validity.Arbitrary
    ( arbitrarySpec
    , arbitraryGeneratesOnlyValid
    ) where

import Data.Data

import Data.GenValidity

import Test.Hspec
import Test.QuickCheck

import Test.Validity.GenValidity
import Test.Validity.Utils

-- | A @Spec@ that specifies that @arbitrary@ only generates data that
-- satisfy @isValid@
--
-- Example usage:
--
-- > arbitrarySpec @Int
arbitrarySpec
    :: forall a.
       (Typeable a, Show a, Validity a, Arbitrary a)
    => Spec
arbitrarySpec = do
    let name = nameOf @a
    describe ("Arbitrary " ++ name) $
        describe ("arbitrary :: Gen " ++ name) $
        it "only generates valid values" $ arbitraryGeneratesOnlyValid @a

-- | @arbitrary@ only generates valid data
--
-- prop> arbitraryGeneratesOnlyValid @Int
arbitraryGeneratesOnlyValid
    :: forall a.
       (Show a, Validity a, Arbitrary a)
    => Property
arbitraryGeneratesOnlyValid = genGeneratesValid @a arbitrary
