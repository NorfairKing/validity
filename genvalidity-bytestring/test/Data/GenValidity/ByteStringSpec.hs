module Data.GenValidity.ByteStringSpec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck

import Data.GenValidity
import Data.GenValidity.ByteString ()

import Data.ByteString (ByteString)

spec :: Spec
spec = do
    describe "genValid" $
        it "generates valid bytestring" $
        forAll (genValid :: Gen ByteString) isValid
    describe "genUnchecked `suchThat` isValid" $
        it "generates valid bytestring" $
        forAll ((genUnchecked :: Gen ByteString) `suchThat` isValid) isValid
