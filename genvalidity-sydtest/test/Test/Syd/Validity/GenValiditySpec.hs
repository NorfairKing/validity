{-# LANGUAGE TypeApplications #-}

module Test.Syd.Validity.GenValiditySpec where

import Test.Syd
import Test.Syd.Validity.GenValidity

spec :: Spec
spec = do
  genValiditySpec @Rational
  genValidSpec @Rational
  genInvalidSpec @Rational
