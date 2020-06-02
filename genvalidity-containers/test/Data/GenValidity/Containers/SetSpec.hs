{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module Data.GenValidity.Containers.SetSpec where

import Test.Hspec

import Data.GenValidity
import Data.GenValidity.Set
import Data.Set (Set)
import Test.Validity.GenValidity

spec :: Spec
spec = do
    describe "genStructurallyValidSetOf" $
        it "produces valid sets" $
        genGeneratesValid
            (genStructurallyValidSetOf @Rational genValid)
    describe "genStructurallyValidSetOfInvalidValues" $
        it "produces valid sets" $
        genGeneratesInvalid
            (genStructurallyValidSetOfInvalidValues @Rational)
#if MIN_VERSION_containers(0,5,9)
    describe "genStructurallyInvalidSet" $
        it "produces invalid sets" $
        genGeneratesInvalid (genStructurallyInvalidSet @Rational)
#endif
    genValidSpec @(Set Int)
    genValiditySpec @(Set Rational)
