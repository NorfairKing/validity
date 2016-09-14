{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Validity.Validity
    ( -- * Tests for Validity instances
      validitySpec

    , validityLawsForGen
    , validityLaws
    ) where

import           Data.Data
import           Data.Proxy

import           Data.GenValidity

import           Test.Hspec
import           Test.QuickCheck

import           Test.Validity.Utils

validitySpec
    :: (Typeable a, Show a, GenValidity a)
    => Proxy a
    -> Spec
validitySpec proxy = do
    let name = nameOf proxy
    describe ("Validity " ++ name) $
        it "is instantiated such that 'isValid' and `isInvalid` are complementary." $
            validityLaws proxy

validityLaws
    :: (Show a, GenValidity a)
    => Proxy a
    -> Property
validityLaws proxy = validityLawsForGen proxy genUnchecked

validityLawsForGen
    :: (Show a, Validity a)
    => Proxy a
    -> Gen a
    -> Property
validityLawsForGen proxy gen =
    forAll gen $ \a ->
        (a `asProxyTypeOf` proxy) `shouldSatisfy`
            (\v -> (isValid v || isInvalid v)
            && not (isValid v && isInvalid v))

