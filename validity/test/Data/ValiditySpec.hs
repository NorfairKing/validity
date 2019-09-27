{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}

module Data.ValiditySpec
  ( spec
  ) where

import GHC.Generics (Generic)
#if !MIN_VERSION_base(4,7,0)
import Data.Monoid
#endif
import Data.Maybe
import Data.Validity
import GHC.Real (Ratio(..), infinity, notANumber)

import Test.Hspec

newtype NormalisedRatio a =
  NormalisedRatio (Ratio a)
  deriving (Show, Eq, Generic)

instance (Validity a, Integral a) => Validity (NormalisedRatio a) where
  validate nr@(NormalisedRatio r) =
    mconcat
      [ genericValidate nr
      , validateRatioNotNaN r
      , validateRatioNotInfinite r
      , validateRatioNormalised r
      ]

data Wrong
  = Wrong
  | Fine
  deriving (Show, Eq)

instance Validity Wrong where
  validate w =
    case w of
      Wrong -> invalid "Wrong"
      Fine -> valid

data GeneratedValidity =
  G Rational Rational
  deriving (Show, Eq, Generic)

instance Validity GeneratedValidity

spec :: Spec
spec = do
  describe "Weird Chars" $ do
    describe "isUtf16SurrogateCodePoint" $ do
      it "Says that a is a valid char" $ isUtf16SurrogateCodePoint 'a' `shouldBe` False
      it "Says that \\55810 is an invalid char" $ isUtf16SurrogateCodePoint '\55810' `shouldBe` True
    describe "validateCharNotUtf16SurrogateCodePoint" $ do
      it "Says that a is a valid char" $
        prettyValidation (validateCharNotUtf16SurrogateCodePoint 'a') `shouldSatisfy` isNothing
      it "Says that \\55810 is an invalid char" $
        prettyValidation (validateCharNotUtf16SurrogateCodePoint '\55810') `shouldSatisfy` isJust
  describe "NormalisedRatio" $ do
    it "says that NaN is invalid" $ NormalisedRatio notANumber `shouldSatisfy` (not . isValid)
    it "says that +Inf is invalid" $ NormalisedRatio infinity `shouldSatisfy` (not . isValid)
    it "says that -Inf is invalid" $ NormalisedRatio (-infinity) `shouldSatisfy` (not . isValid)
    it "says that these non-normalised numbers are invalid" $ do
      NormalisedRatio ((5 :: Integer) :% 5) `shouldSatisfy` (not . isValid)
      NormalisedRatio ((1 :: Integer) :% (-5)) `shouldSatisfy` (not . isValid)
      NormalisedRatio ((6 :: Integer) :% 2) `shouldSatisfy` (not . isValid)
      NormalisedRatio ((2 :: Integer) :% 6) `shouldSatisfy` (not . isValid)
      NormalisedRatio ((2 :: Integer) :% 0) `shouldSatisfy` (not . isValid)
      NormalisedRatio ((0 :: Integer) :% 5) `shouldSatisfy` (not . isValid)
      NormalisedRatio ((0 :: Integer) :% 0) `shouldSatisfy` (not . isValid)
  describe "Wrong" $ do
    it "says Wrong is invalid" $ Wrong `shouldSatisfy` (not . isValid)
    it "says Fine is valid" $ Fine `shouldSatisfy` isValid
  describe "GeneratedValidity" $ do
    let nan = 1 :% 0
    it "says G (1:%0) 0 is not valid" $ G nan 0 `shouldSatisfy` (not . isValid)
    it "says G 0 (1:%0) is not valid" $ G 0 nan `shouldSatisfy` (not . isValid)
    it "says G 0 0 is valid" $ G 0 0 `shouldSatisfy` isValid
