{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for GenRelativeValidity instances
module Test.Validity.GenRelativeValidity
    ( genRelativeValiditySpec
    , genRelativeValidSpec
    , genRelativeInvalidSpec
    , genRelativeValidGeneratesValid
    , genRelativeInvalidGeneratesInvalid
    ) where

import Data.Data
import Data.Proxy

import Data.GenRelativeValidity
import Data.GenValidity

import Test.Hspec
import Test.QuickCheck

import Test.Validity.Utils

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
    :: ( Typeable a
       , Typeable b
       , Show a
       , Show b
       , GenValid a
       , GenValid b
       , RelativeValidity a b
       , GenRelativeValid a b
       , GenRelativeInvalid a b
       )
    => Proxy a -> Proxy b -> Spec
genRelativeValiditySpec one two = do
    genRelativeValidSpec one two
    genRelativeValidSpec one two

genRelativeValidSpec
    :: ( Typeable a
       , Typeable b
       , Show a
       , Show b
       , GenValid a
       , GenValid b
       , RelativeValidity a b
       , GenRelativeValid a b
       )
    => Proxy a -> Proxy b -> Spec
genRelativeValidSpec one two =
    parallel $ do
        let nameOne = nameOf one
        let nameTwo = nameOf two
        describe ("GenRelativeValidity " ++ nameOne ++ " " ++ nameTwo) $ do
            describe ("genValidFor   :: " ++ nameTwo ++ " -> Gen " ++ nameOne) $
                it
                    ("only generates valid \'" ++
                     nameOne ++ "\'s for the " ++ nameTwo) $
                genRelativeValidGeneratesValid one two

genRelativeInvalidSpec
    :: ( Typeable a
       , Typeable b
       , Show a
       , Show b
       , GenValid a
       , GenValid b
       , RelativeValidity a b
       , GenRelativeInvalid a b
       )
    => Proxy a -> Proxy b -> Spec
genRelativeInvalidSpec one two =
    parallel $ do
        let nameOne = nameOf one
        let nameTwo = nameOf two
        describe ("GenRelativeInvalid " ++ nameOne ++ " " ++ nameTwo) $ do
            describe ("genInvalidFor   :: " ++ nameTwo ++ " -> Gen " ++ nameOne) $
                it
                    ("only generates invalid \'" ++
                     nameOne ++ "\'s for the " ++ nameTwo) $
                genRelativeInvalidGeneratesInvalid one two

-- | @genValidFor b@ only generates values that satisfy @isValidFor b@
genRelativeValidGeneratesValid
    :: (Show a, Show b, GenValid b, RelativeValidity a b, GenRelativeValid a b)
    => Proxy a -> Proxy b -> Property
genRelativeValidGeneratesValid one two =
    forAll genValid $ \b ->
        forAll (genValidFor b) $ \a ->
            (a `asProxyTypeOf` one) `shouldSatisfy`
            (`isValidFor` (b `asProxyTypeOf` two))

-- | @genInvalidFor b@ only generates values that do not satisfy @isValidFor b@
genRelativeInvalidGeneratesInvalid
    :: ( Show a
       , Show b
       , GenUnchecked b
       , RelativeValidity a b
       , GenRelativeInvalid a b
       )
    => Proxy a -> Proxy b -> Property
genRelativeInvalidGeneratesInvalid one two =
    forAll genUnchecked $ \b ->
        forAll (genInvalidFor b) $ \a ->
            (a `asProxyTypeOf` one) `shouldNotSatisfy`
            (`isValidFor` (b `asProxyTypeOf` two))
