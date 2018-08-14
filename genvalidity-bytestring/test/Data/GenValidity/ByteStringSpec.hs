module Data.GenValidity.ByteStringSpec
    ( spec
    ) where

import Control.DeepSeq

import Test.Hspec
import Test.QuickCheck

import Data.GenValidity
import Data.GenValidity.ByteString ()

import qualified Data.ByteString as SB (ByteString)
import qualified Data.ByteString.Lazy as LB (ByteString)

checkable :: (Validity t, Show t, NFData t) => Gen t -> SpecWith ()
checkable gen =
    it "generates bytestrings that can be checked for validity" $
    forAll gen $ \v ->
        case prettyValidation v of
            Left e -> deepseq e True
            Right v_ -> deepseq v_ True

showable :: Show t => Gen t -> SpecWith ()
showable gen =
    it "generates bytestrings that can be shown" $
    forAll gen $ \v -> deepseq (show v) True

spec :: Spec
spec = do
    describe "genUnchecked :: Gen SB.ByteString" $ do
        checkable (genUnchecked :: Gen SB.ByteString)
        -- showable (genUnchecked :: Gen SB.ByteString) DOES NOT HOLD
    describe "genValid :: Gen SB.ByteString" $ do
        checkable (genValid :: Gen SB.ByteString)
        showable (genValid :: Gen SB.ByteString)
        it "generates valid strict bytestring" $
            forAll (genValid :: Gen SB.ByteString) isValid
    describe "genInvalid :: Gen SB.ByteString" $ do
        checkable (genInvalid :: Gen SB.ByteString)
        -- showable (genInvalid :: Gen SB.ByteString) DOES NOT HOLD
        it "generates invalid strict bytestring" $
            forAll (genInvalid :: Gen SB.ByteString) isInvalid
    describe "genUnchecked :: Gen LB.ByteString" $ do
        checkable (genUnchecked :: Gen LB.ByteString)
        -- showable (genUnchecked :: Gen LB.ByteString) DOES NOT HOLD
    describe "genValid :: Gen LB.ByteString" $ do
        checkable (genValid :: Gen LB.ByteString)
        showable (genValid :: Gen LB.ByteString)
        it "generates valid lazy bytestring" $
            forAll (genValid :: Gen LB.ByteString) isValid
    describe "genInvalid :: Gen LB.ByteString" $ do
        checkable (genInvalid :: Gen LB.ByteString)
        -- showable (genInvalid :: Gen LB.ByteString) DOES NOT HOLD
        it "generates invalid lazy bytestring" $
            forAll (genInvalid :: Gen LB.ByteString) isInvalid
