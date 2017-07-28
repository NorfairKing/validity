module Data.GenValidity.ScientificSpec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck

import Data.GenValidity
import Data.GenValidity.Scientific ()

import Data.Scientific

spec :: Spec
spec =
    describe "genValid" $
        it "generates valid Scientific's" $
        forAll (genValid :: Gen Scientific) isValid
