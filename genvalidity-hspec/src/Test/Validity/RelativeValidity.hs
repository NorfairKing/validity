{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Validity.RelativeValidity
    ( -- * Tests for RelativeValidity instances
      relativeValiditySpec
    , relativeValidityLaws
    , relativeValidityLawsForGens
    , relativeValidityImpliesValidA
    , relativeValidityImpliesValidB
    ) where

import           Data.Data
import           Data.Proxy

import           Data.GenRelativeValidity
import           Data.GenValidity

import           Test.Hspec
import           Test.QuickCheck

import           Test.Validity.Utils

-- | A @Spec@ that specifies that @isValidFor@ implies @isValid@
--
-- In general it is a good idea to add this spec to your test suite if
-- the @a@ and @b@ in @RelativeValidity a b@ also have a @Validity@ instance.
--
-- Example usage:
--
-- > relativeValiditySpec
-- >     (Proxy :: Proxy MyDataFor)
-- >     (Proxy :: Proxy MyOtherData)
relativeValiditySpec
    :: (Typeable a, Typeable b,
        Show a, Show b,
        GenValidity a, GenValidity b, GenRelativeValidity a b)
    => Proxy a
    -> Proxy b
    -> Spec
relativeValiditySpec one two = do
    let nameOne = nameOf one
        nameTwo = nameOf two
    describe ("RelativeValidity " ++ nameOne ++ " " ++ nameTwo) $ do
        it ("is instantiated such that 'isValidFor'"
            ++ "and 'isInvalidFor' are complementary") $
            relativeValidityLaws one two

        describe ("isValidFor :: "
                  ++ nameOne
                  ++ " -> "
                  ++ nameTwo
                  ++ " -> Bool") $ do
            it ("implies isValid " ++ nameOne ++ " for any " ++ nameTwo) $
                relativeValidityImpliesValidA one two
            it ("implies isValid " ++ nameTwo ++ " for any " ++ nameOne) $
                relativeValidityImpliesValidB one two

relativeValidityLaws
    :: (Show a, Show b, GenValidity a, GenValidity b, RelativeValidity a b)
    => Proxy a
    -> Proxy b
    -> Property
relativeValidityLaws proxya proxyb
    = relativeValidityLawsForGens proxya proxyb genUnchecked genUnchecked

relativeValidityLawsForGens
    :: (Show a, Show b, RelativeValidity a b)
    => Proxy a
    -> Proxy b
    -> Gen a
    -> Gen b
    -> Property
relativeValidityLawsForGens proxya proxyb gena genb =
    forAll gena $ \a ->
        forAll genb $ \b ->
            (a `asProxyTypeOf` proxya, b `asProxyTypeOf` proxyb)
                `shouldSatisfy`
                    (\(v, w) -> (isValidFor v w || isInvalidFor v w)
                    && not (isValidFor v w && isInvalidFor v w))

-- | @isValidFor a b@ implies @isValid a@ for all @b@
relativeValidityImpliesValidA
    :: (Show a, Show b,
        GenValidity a, GenValidity b, RelativeValidity a b)
    => Proxy a
    -> Proxy b
    -> Property
relativeValidityImpliesValidA one two =
    forAll genUnchecked $ \a ->
        forAll genUnchecked $ \b ->
            not ((a `asProxyTypeOf` one) `isValidFor` (b `asProxyTypeOf` two))
            || isValid a

-- | @isValidFor a b@ implies @isValid b@ for all @a@
relativeValidityImpliesValidB
    :: (Show a, Show b,
        GenValidity a, GenValidity b, RelativeValidity a b)
    => Proxy a
    -> Proxy b
    -> Property
relativeValidityImpliesValidB one two =
    forAll genUnchecked $ \a ->
        forAll genUnchecked $ \b ->
            not ((a `asProxyTypeOf` one) `isValidFor` (b `asProxyTypeOf` two))
              || isValid b

