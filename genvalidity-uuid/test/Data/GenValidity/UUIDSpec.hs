module Data.GenValidity.UUIDSpec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck

import Data.GenValidity
import Data.GenValidity.UUID ()

import Data.UUID

spec :: Spec
spec =
    describe "genValid" $
    it "generates valid UUId's" $ forAll (genValid :: Gen UUID) isValid
