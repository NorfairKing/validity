{-# LANGUAGE TypeApplications #-}

module Test.Validity.GenValiditySpec where

import Test.Hspec
import Test.Validity.GenValidity

spec :: Spec
spec = do
  genValiditySpec @Rational
  genValidSpec @Rational
  genInvalidSpec @Rational
