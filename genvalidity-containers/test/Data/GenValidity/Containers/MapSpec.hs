{-# LANGUAGE TypeApplications #-}

module Data.GenValidity.Containers.MapSpec where

import Data.GenValidity
import Data.GenValidity.Map
import Data.Map (Map)
import Test.Hspec
import Test.Validity.GenValidity

spec :: Spec
spec = do
  describe "genMapOf" $
    it "produces valid maps" $
      genGeneratesValid
        (genMapOf @Rational @Rational genValid)
  genValidSpec @(Map Int Rational)
  genValidSpec @(Map Rational Rational)
