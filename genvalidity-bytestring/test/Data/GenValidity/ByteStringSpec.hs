module Data.GenValidity.ByteStringSpec
  ( spec,
  )
where

import Control.DeepSeq
import qualified Data.ByteString as SB (ByteString)
import qualified Data.ByteString.Lazy as LB (ByteString)
import qualified Data.ByteString.Short as Short (ShortByteString)
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Test.Hspec
import Test.QuickCheck

checkable :: (Validity t, Show t, NFData t) => Gen t -> SpecWith ()
checkable gen =
  it "generates bytestrings that can be checked for validity" $
    forAll gen $ \v ->
      case prettyValidate v of
        Left e -> deepseq e True
        Right v_ -> deepseq v_ True

showable :: (Show t) => Gen t -> SpecWith ()
showable gen =
  it "generates bytestrings that can be shown" $
    forAll gen $
      \v -> deepseq (show v) True

spec :: Spec
spec = do
  do
    describe "genValid :: Gen SB.ByteString" $ do
      checkable (genValid :: Gen SB.ByteString)
      showable (genValid :: Gen SB.ByteString)
      it "generates valid strict bytestring" $
        forAll (genValid :: Gen SB.ByteString) isValid
    describe "genValid :: Gen LB.ByteString" $ do
      checkable (genValid :: Gen LB.ByteString)
      showable (genValid :: Gen LB.ByteString)
      it "generates valid lazy bytestring" $
        forAll (genValid :: Gen LB.ByteString) isValid
  describe "genValid :: Gen Short.ShortByteString" $ do
    checkable (genValid :: Gen Short.ShortByteString)
    showable (genValid :: Gen Short.ShortByteString)
  describe "genValid :: Gen Short.ShortByteString" $ do
    checkable (genValid :: Gen Short.ShortByteString)
    showable (genValid :: Gen Short.ShortByteString)
    it "generates valid lazy bytestring" $
      forAll (genValid :: Gen Short.ShortByteString) isValid

-- Uncomment to test that the instances are poisoned
--
-- instance GenValid SB.ByteString where
--     genValid = undefined
--     shrinkValid = undefined
--
-- instance GenValid LB.ByteString where
--     genValid = undefined
--     shrinkValid = undefined
--
-- Uncomment to see the error message that you would get
--
-- data FooBar = FooBar SB.ByteString String
--     deriving (Show, Eq, Generic)
-- instance Validity FooBar
-- instance GenValid FooBar
