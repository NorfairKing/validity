{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Validity.Arbitrary
    ( -- * Tests for Arbitrary instances involving Validity
      arbitrarySpec
    , arbitraryGeneratesOnlyValid
    , shrinkProducesOnlyValids
    ) where

import           Data.Data

import           Data.GenValidity

import           Test.Hspec
import           Test.QuickCheck

import           Test.Validity.GenValidity
import           Test.Validity.Utils

-- | A @Spec@ that specifies that @arbitrary@ only generates data that
-- satisfy @isValid@
--
-- Example usage:
--
-- > arbitrarySpec (Proxy :: Proxy MyData)
arbitrarySpec
    :: (Typeable a, Show a, Validity a, Arbitrary a)
    => Proxy a
    -> Spec
arbitrarySpec proxy = do
    let name = nameOf proxy
    describe ("Arbitrary " ++ name) $
        describe ("arbitrary :: Gen " ++ name) $
            it "only generates valid values" $
                arbitraryGeneratesOnlyValid proxy

-- | @arbitrary@ only generates valid data
arbitraryGeneratesOnlyValid
    :: forall a. (Show a, Validity a, Arbitrary a)
    => Proxy a
    -> Property
arbitraryGeneratesOnlyValid _ =
    genGeneratesValid (arbitrary :: Gen a)

-- | @shrink@, applied to valid data only produces valid data
shrinkProducesOnlyValids
    :: forall a. (Show a, Validity a, Arbitrary a)
    => Proxy a
    -> Property
shrinkProducesOnlyValids _ =
    genGeneratesValid (shrink <$> arbitrary :: Gen [a])
