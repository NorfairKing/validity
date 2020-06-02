{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module Data.GenValidity.Containers.MapSpec where

import Test.Hspec

import Data.GenValidity
import Data.GenValidity.Map
import Data.Map (Map)
import Test.Validity.GenValidity

spec :: Spec
spec = do
    describe "genStructurallyValidMapOf" $
        it "produces valid maps" $
        genGeneratesValid
            (genStructurallyValidMapOf @Rational @Rational genValid)
    describe "genStructurallyValidMapOfInvalidValues" $
        it "produces valid maps" $
        genGeneratesInvalid
            (genStructurallyValidMapOfInvalidValues @Rational @Rational)
#if MIN_VERSION_containers(0,5,9)
    describe "genStructurallyInvalidMap" $
        it "produces invalid maps" $
        genGeneratesInvalid
            (genStructurallyInvalidMap @Rational @Rational)
#endif
    genValidSpec @(Map Int Rational)
    genValiditySpec @(Map Rational Rational)
