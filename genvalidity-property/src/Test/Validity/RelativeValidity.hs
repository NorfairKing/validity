{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for RelativeValidity instances
--
-- You will need @TypeApplications@ to use these.
module Test.Validity.RelativeValidity
    ( relativeValidityImpliesValidA
    , relativeValidityImpliesValidB
    ) where

import Data.Data

import Data.GenRelativeValidity
import Data.GenValidity

import Test.Hspec
import Test.QuickCheck

import Test.Validity.Utils

-- | @isValidFor a b@ implies @isValid a@ for all @b@
relativeValidityImpliesValidA
    :: forall a b.
       ( Show a
       , Show b
       , Validity a
       , GenUnchecked a
       , GenUnchecked b
       , RelativeValidity a b
       )
    => Property
relativeValidityImpliesValidA =
    forAll genUnchecked $ \(a :: a) ->
        forAll genUnchecked $ \(b :: b) -> (a `isValidFor` b) ===> isValid a

-- | @isValidFor a b@ implies @isValid b@ for all @a@
relativeValidityImpliesValidB
    :: forall a b.
       ( Show a
       , Show b
       , Validity b
       , GenUnchecked a
       , GenUnchecked b
       , RelativeValidity a b
       )
    => Property
relativeValidityImpliesValidB =
    forAll genUnchecked $ \(a :: a) ->
        forAll genUnchecked $ \(b :: b) -> (a `isValidFor` b) ===> isValid b
