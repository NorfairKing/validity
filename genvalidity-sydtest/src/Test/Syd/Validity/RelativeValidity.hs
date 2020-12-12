{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for RelativeValidity instances
--
-- You will need @TypeApplications@ to use these.
module Test.Syd.Validity.RelativeValidity
    ( relativeValiditySpec
    , relativeValidityImpliesValidA
    , relativeValidityImpliesValidB
    ) where

import Data.Data

import Data.GenRelativeValidity
import Data.GenValidity

import Test.Syd
import Test.QuickCheck

import Test.Syd.Validity.Property.Utils
import Test.Syd.Validity.Utils

-- | A @Spec@ that specifies that @isValidFor@ implies @isValid@
--
-- In general it is a good idea to add this spec to your test suite if
-- the @a@ and @b@ in @RelativeValidity a b@ also have a @Validity@ instance.
--
-- Example usage:
--
-- > relativeValiditySpec @MyDataFor @MyOtherData
relativeValiditySpec ::
       forall a b.
       ( Typeable a
       , Typeable b
       , Show a
       , Show b
       , Validity a
       , Validity b
       , GenUnchecked a
       , GenUnchecked b
       , RelativeValidity a b
       )
    => Spec
relativeValiditySpec =
    parallel $ do
        let nameOne = nameOf @a
            nameTwo = nameOf @b
        describe ("RelativeValidity " ++ nameOne ++ " " ++ nameTwo) $
            describe
                ("isValidFor :: " ++ nameOne ++ " -> " ++ nameTwo ++ " -> Bool") $ do
                it ("implies isValid " ++ nameOne ++ " for any " ++ nameTwo) $
                    relativeValidityImpliesValidA @a @b
                it ("implies isValid " ++ nameTwo ++ " for any " ++ nameOne) $
                    relativeValidityImpliesValidB @a @b

-- | @isValidFor a b@ implies @isValid a@ for all @b@
relativeValidityImpliesValidA ::
       forall a b.
       ( Show a
       , Show b
       , Validity a
       , GenUnchecked a
       , GenUnchecked b
       , RelativeValidity a b
       )
    => Property
relativeValidityImpliesValidA =
    forAllUnchecked $ \(a :: a) ->
        forAllUnchecked $ \(b :: b) -> (a `isValidFor` b) ===> isValid a

-- | @isValidFor a b@ implies @isValid b@ for all @a@
relativeValidityImpliesValidB ::
       forall a b.
       ( Show a
       , Show b
       , Validity b
       , GenUnchecked a
       , GenUnchecked b
       , RelativeValidity a b
       )
    => Property
relativeValidityImpliesValidB =
    forAllUnchecked $ \(a :: a) ->
        forAllUnchecked $ \(b :: b) -> (a `isValidFor` b) ===> isValid b
