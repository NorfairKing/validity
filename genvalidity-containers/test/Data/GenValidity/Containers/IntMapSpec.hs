{-# LANGUAGE TypeApplications #-}

module Data.GenValidity.Containers.IntMapSpec where

import Data.GenValidity
import Data.GenValidity.IntMap
import Data.IntMap (IntMap)
import Test.Hspec
import Test.Validity.GenValidity

spec :: Spec
spec = do
  describe "genIntMapOf" $
    it "produces valid maps" $
      genGeneratesValid
        (genIntMapOf @Rational genValid)
  genValidSpec @(IntMap Rational)
  genValidSpec @(IntMap Rational)
