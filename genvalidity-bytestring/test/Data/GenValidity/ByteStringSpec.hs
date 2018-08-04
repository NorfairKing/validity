module Data.GenValidity.ByteStringSpec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck

import Data.GenValidity
import Data.GenValidity.ByteString ()

import qualified Data.ByteString as SB (ByteString)

spec :: Spec
spec = do
    describe "genValid" $
        it "generates valid bytestring" $
        forAll (genValid :: Gen SB.ByteString) isValid
    describe "genInvalid" $
        it "generates invalid bytestring" $
        forAll (genInvalid :: Gen SB.ByteString) isInvalid
