{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module Test.Validity.Containers.SetSpec where

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
            (const [])
    describe "genStructurallyValidSetOfInvalidValues" $
        it "produces valid sets" $
        genGeneratesInvalid
            (genStructurallyValidSetOfInvalidValues @Rational)
            (const [])
#if MIN_VERSION_containers(0,5,9)
    describe "genStructurallyInvalidSet" $
        it "produces invalid sets" $
        genGeneratesInvalid (genStructurallyInvalidSet @Rational) (const [])
#endif
    genValidSpec @(Set Int)
    genValiditySpec @(Set Rational)
