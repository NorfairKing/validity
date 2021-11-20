{-# LANGUAGE TypeApplications #-}

module Test.Validity.GenValiditySpec where

import Test.Hspec
import Test.Validity.GenValidity

spec :: Spec
spec = do
  genValidSpec @Rational
  genValidSpec @Rational
