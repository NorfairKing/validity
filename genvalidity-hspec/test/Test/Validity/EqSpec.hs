{-# LANGUAGE TypeApplications #-}

module Test.Validity.EqSpec where

import Test.Hspec

import Data.GenValidity
import Test.Validity.Eq
import Test.Validity.TestUtils

spec :: Spec
spec = do
    eqSpecOnValid @Double
    eqSpec @Int
    failsBecause "reflexivity does not hold for NaN" $ eqSpecOnInvalid @Double
    eqSpecOnArbitrary @Int
    eqSpecOnGen ((* 2) <$> genValid @Int) "even"
