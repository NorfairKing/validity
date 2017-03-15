{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for Arbitrary instances involving Validity
module Test.Validity.Arbitrary
    ( arbitraryGeneratesOnlyValid
    ) where

import Data.Data

import Data.GenValidity

import Test.Hspec
import Test.QuickCheck

import Test.Validity.GenValidity
import Test.Validity.Utils

-- | @arbitrary@ only generates valid data
--
-- prop> arbitraryGeneratesOnlyValid @Int
arbitraryGeneratesOnlyValid
    :: forall a.
       (Show a, Validity a, Arbitrary a)
    => Property
arbitraryGeneratesOnlyValid = genGeneratesValid @a arbitrary
