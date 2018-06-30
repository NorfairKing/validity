{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module Test.Validity.Containers.MapSpec where

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
            (genStructurallyValidMapOf @Double @Double genValid)
            (const [])
    describe "genStructurallyValidMapOfInvalidValues" $
        it "produces valid maps" $
        genGeneratesInvalid
            (genStructurallyValidMapOfInvalidValues @Double @Double)
            (const [])
#if MIN_VERSION_containers(0,5,9)
    describe "genStructurallyInvalidMap" $
        it "produces invalid maps" $
        genGeneratesInvalid
            (genStructurallyInvalidMap @Double @Double)
            (const [])
#endif
    genValidSpec @(Map Int Double)
    genValiditySpec @(Map Double Double)
