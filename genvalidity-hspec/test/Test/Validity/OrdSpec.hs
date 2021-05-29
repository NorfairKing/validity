{-# LANGUAGE TypeApplications #-}

module Test.Validity.OrdSpec where

import Data.GenValidity
import Test.Hspec
import Test.Validity.Ord
import Test.Validity.Utils

spec :: Spec
spec = do
  ordSpecOnValid @Rational
  failsBecause "NaN >= NaN is False" $ ordSpec @Double
  ordSpec @Int
  ordSpecOnArbitrary @Int
  ordSpecOnGen ((* 2) <$> genValid @Int) "even" (const [])
