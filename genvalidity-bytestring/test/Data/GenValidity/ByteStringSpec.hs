module Data.GenValidity.ByteStringSpec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck

import Data.GenValidity
import Data.GenValidity.ByteString ()

import qualified Data.ByteString as SB (ByteString)
import qualified Data.ByteString.Lazy as LB (ByteString)

spec :: Spec
spec = do
    describe "genUnchecked :: Gen SB.ByteString" $
        it "generates strict bytestrings that can be checked for validity" $
        forAll (genUnchecked :: Gen SB.ByteString) $ \v ->
            case prettyValidation v of
                Left e -> seq e True
                Right v_ -> seq v_ True
    describe "genValid :: Gen SB.ByteString" $
        it "generates valid strict bytestring" $
        forAll (genValid :: Gen SB.ByteString) isValid
    describe "genInvalid :: Gen SB.ByteString" $
        it "generates invalid strict bytestring" $
        forAll (genInvalid :: Gen SB.ByteString) isInvalid
    describe "genUnchecked :: Gen LB.ByteString" $
        it "generates lazy bytestrings that can be checked for validity" $
        forAll (genUnchecked :: Gen LB.ByteString) $ \v ->
            case prettyValidation v of
                Left e -> seq e True
                Right v_ -> seq v_ True
    describe "genValid :: Gen LB.ByteString" $
        it "generates valid lazy bytestring" $
        forAll (genValid :: Gen LB.ByteString) isValid
    describe "genInvalid :: Gen LB.ByteString" $
        it "generates invalid lazy bytestring" $
        forAll (genInvalid :: Gen LB.ByteString) isInvalid
