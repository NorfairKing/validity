{-# LANGUAGE TypeApplications #-}

module Test.Syd.Validity.GenValiditySpec where

import Test.Syd
import Test.Syd.Validity.GenValidity

spec :: Spec
spec = do
  genValidSpec @Rational
  genValidSpec @Rational
