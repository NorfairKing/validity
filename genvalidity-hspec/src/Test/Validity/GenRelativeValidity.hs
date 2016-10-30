{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Validity.GenRelativeValidity
    ( -- * Tests for GenRelativeValidity instances
      genRelativeValiditySpec
    , genRelativeValidityValidGeneratesValid
    , genRelativeValidityInvalidGeneratesInvalid
    ) where

import           Data.Data
import           Data.Proxy

import           Data.GenRelativeValidity
import           Data.GenValidity

import           Test.Hspec
import           Test.QuickCheck

import           Test.Validity.Utils

-- | A @Spec@ that specifies that @genValidFor@ and @genInvalidFor@ work as
-- intended.
--
-- In general it is a good idea to add this spec to your test suite if you
-- write a custom implementation of @genValidFor@ or @genInvalidFor@.
--
-- Example usage:
--
-- > relativeGenValiditySpec (proxy :: MyDataFor) (proxy :: MyOtherData)
genRelativeValiditySpec
    :: (Typeable a, Typeable b,
        Show a, Show b,
        GenValidity a, GenValidity b,
        RelativeValidity a b,
        GenRelativeValidity a b)
    => Proxy a
    -> Proxy b
    -> Spec
genRelativeValiditySpec one two = parallel $ do
    let nameOne = nameOf one
    let nameTwo = nameOf two
    describe ("GenRelativeValidity " ++ nameOne ++ " " ++ nameTwo) $ do
        describe ("genValidFor   :: " ++ nameTwo ++ " -> Gen " ++ nameOne) $
            it ("only generates valid \'"
                ++ nameOne
                ++ "\'s for the "
                ++ nameTwo) $
                genRelativeValidityValidGeneratesValid one two

        describe ("genInvalidFor :: " ++ nameTwo ++ " -> Gen " ++ nameOne) $
            it ("only generates invalid \'"
                ++ nameOne
                ++ "\'s for the "
                ++ nameTwo) $
                genRelativeValidityInvalidGeneratesInvalid one two

-- | @genValidFor b@ only generates values that satisfy @isValidFor b@
genRelativeValidityValidGeneratesValid
    :: (Show a, Show b,
        GenValidity a, GenValidity b,
        RelativeValidity a b,
        GenRelativeValidity a b)
    => Proxy a
    -> Proxy b
    -> Property
genRelativeValidityValidGeneratesValid one two =
    forAll genValid $ \b ->
        forAll (genValidFor b) $ \a ->
            (a `asProxyTypeOf` one)
                `shouldSatisfy` (`isValidFor` (b `asProxyTypeOf` two))

-- | @genInvalidFor b@ only generates values that do not satisfy @isValidFor b@
genRelativeValidityInvalidGeneratesInvalid
    :: (Show a, Show b,
        GenValidity a, GenValidity b,
        RelativeValidity a b,
        GenRelativeValidity a b)
    => Proxy a
    -> Proxy b
    -> Property
genRelativeValidityInvalidGeneratesInvalid one two =
    forAll genUnchecked $ \b ->
        forAll (genInvalidFor b) $ \a ->
            (a `asProxyTypeOf` one)
                `shouldNotSatisfy` (`isValidFor` (b `asProxyTypeOf` two))
