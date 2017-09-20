{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for GenValidity instances
module Test.Validity.GenValidity.Property
    ( genGeneratesValid
    , genGeneratesInvalid
    ) where

import Data.GenValidity

import Test.Hspec
import Test.QuickCheck

-- | The given generator generates only valid data points
genGeneratesValid ::
       forall a. (Show a, Validity a)
    => Gen a
    -> (a -> [a])
    -> Property
genGeneratesValid gen s = forAllShrink gen s (`shouldSatisfy` isValid)

-- | The given generator generates only invalid data points
genGeneratesInvalid ::
       forall a. (Show a, Validity a)
    => Gen a
    -> (a -> [a])
    -> Property
genGeneratesInvalid gen s = forAllShrink gen s (`shouldSatisfy` isInvalid)
