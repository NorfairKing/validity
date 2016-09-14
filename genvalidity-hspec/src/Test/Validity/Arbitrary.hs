{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Validity.Arbitrary
    ( -- * Tests for Arbitrary instances involving Validity
      arbitrarySpec
    , arbitraryGeneratesOnlyValid
    , shrinkProducesOnlyValids
    ) where

import           Data.Data
import           Data.Proxy

import           Data.GenValidity

import           Test.Hspec
import           Test.QuickCheck

import           Test.Validity.Utils
import           Test.Validity.GenValidity

-- | A @Spec@ that specifies that @arbitrary@ only generates data that
-- satisfy @isValid@ and that @shrink@ only produces data that satisfy
-- @isValid@.
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
    describe ("Arbitrary " ++ name) $ do
        it ("is instantiated such that 'arbitrary' only generates valid \'"
            ++ name
            ++ "\'s.") $
            arbitraryGeneratesOnlyValid proxy

        it ("is instantiated such that 'shrink' only produces valid \'"
            ++ name
            ++ "\'s.") $
            forAll arbitrary $ \a ->
                shrink (a `asProxyTypeOf` proxy) `shouldSatisfy` all isValid

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
