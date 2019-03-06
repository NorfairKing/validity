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
        case prettyValidate v of
            Left e -> deepseq e True
            Right v_ -> deepseq v_ True

showable :: Show t => Gen t -> SpecWith ()
showable gen =
    it "generates bytestrings that can be shown" $
    forAll gen $ \v -> deepseq (show v) True

spec :: Spec
spec = do
    do describe "genValid :: Gen SB.ByteString" $ do
           checkable (genValid :: Gen SB.ByteString)
           showable (genValid :: Gen SB.ByteString)
           it "generates valid strict bytestring" $
               forAll (genValid :: Gen SB.ByteString) isValid
       describe "genValid :: Gen LB.ByteString" $ do
           checkable (genValid :: Gen LB.ByteString)
           showable (genValid :: Gen LB.ByteString)
           it "generates valid lazy bytestring" $
               forAll (genValid :: Gen LB.ByteString) isValid
