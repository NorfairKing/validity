{-# LANGUAGE TypeApplications #-}

module Test.Validity.OrdSpec where

import Test.Hspec

import Data.GenValidity
import Test.Validity.Ord
import Test.Validity.Utils

spec :: Spec
spec = do
    ordSpecOnValid @Double
    failsBecause "NaN >= NaN is False" $ ordSpecOnInvalid @Double
    ordSpec @Int
    ordSpecOnArbitrary @Int
    ordSpecOnGen ((* 2) <$> genValid @Int) "even"
