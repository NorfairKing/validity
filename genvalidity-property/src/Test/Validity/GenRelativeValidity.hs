{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for GenRelativeValidity instances
--
-- You will need @TypeApplications@ to use these.
module Test.Validity.GenRelativeValidity
    ( genRelativeValidGeneratesValid
    , genRelativeInvalidGeneratesInvalid
    ) where

import Data.Data

import Data.GenRelativeValidity
import Data.GenValidity

import Test.Hspec
import Test.QuickCheck

import Test.Validity.Utils

-- | @genValidFor b@ only generates values that satisfy @isValidFor b@
genRelativeValidGeneratesValid
    :: forall a b.
       (Show a, Show b, GenValid b, RelativeValidity a b, GenRelativeValid a b)
    => Property
genRelativeValidGeneratesValid =
    forAll genValid $ \(b :: b) ->
        forAll (genValidFor b) $ \(a :: a) -> a `shouldSatisfy` (`isValidFor` b)

-- | @genInvalidFor b@ only generates values that do not satisfy @isValidFor b@
genRelativeInvalidGeneratesInvalid
    :: forall a b.
       ( Show a
       , Show b
       , GenUnchecked b
       , RelativeValidity a b
       , GenRelativeInvalid a b
       )
    => Property
genRelativeInvalidGeneratesInvalid =
    forAll genUnchecked $ \(b :: b) ->
        forAll (genInvalidFor b) $ \(a :: a) ->
            a `shouldNotSatisfy` (`isValidFor` b)
