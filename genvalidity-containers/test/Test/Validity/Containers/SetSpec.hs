{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module Test.Validity.Containers.SetSpec where

import Test.Hspec

import Data.GenValidity.Set
import Data.Set (Set)
import Test.Validity.GenValidity

spec :: Spec
spec = do
    genValidSpec @(Set Int)
    genValiditySpec @(Set Double)
#if MIN_VERSION_containers(0,5,9)
    describe "genStructurallyInvalidSet" $
        it "produces invalid sets" $
        genGeneratesInvalid
            (genStructurallyInvalidSet @Double)
            (const [])
#endif
