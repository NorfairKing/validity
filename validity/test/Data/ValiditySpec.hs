{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MagicHash #-}

module Data.ValiditySpec
  ( spec,
  )
where

import Data.Maybe
import Data.Validity
import GHC.Exts (Char (..), chr#)
import GHC.Generics (Generic)
import GHC.Int (Int16 (..), Int32 (..), Int8 (..))
import GHC.Real (Ratio (..), infinity, notANumber)
import GHC.Word (Word16 (..), Word32 (..), Word8 (..))
import Test.Hspec

newtype NormalisedRatio a
  = NormalisedRatio (Ratio a)
  deriving (Show, Eq, Generic)

instance (Validity a, Integral a) => Validity (NormalisedRatio a) where
  validate nr@(NormalisedRatio r) =
    mconcat
      [ genericValidate nr,
        validateRatioNotNaN r,
        validateRatioNotInfinite r,
        validateRatioNormalised r
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

data GeneratedValidity
  = G Rational Rational
  deriving (Show, Eq, Generic)

instance Validity GeneratedValidity

spec :: Spec
spec = do
  describe "Small numbers" $ do
    describe "Validity Int8" $ do
      it "Says that minBound is valid" $ isValid (minBound :: Int8) `shouldBe` True
      it "Says that maxBound is valid" $ isValid (maxBound :: Int8) `shouldBe` True
      it "Says that Int# 200 is invalid" $ isValid (I8# 200#) `shouldBe` False
      it "Says that Int# -200 is invalid" $ isValid (I8# (-200#)) `shouldBe` False
    describe "Validity Int16" $ do
      it "Says that minBound is valid" $ isValid (minBound :: Int16) `shouldBe` True
      it "Says that maxBound is valid" $ isValid (maxBound :: Int16) `shouldBe` True
      it "Says that Int# 4000 is invalid" $ isValid (I16# 40000#) `shouldBe` False
      it "Says that Int# -4000 is invalid" $ isValid (I16# (-40000#)) `shouldBe` False
    describe "Validity Int32" $ do
      it "Says that minBound is valid" $ isValid (minBound :: Int32) `shouldBe` True
      it "Says that maxBound is valid" $ isValid (maxBound :: Int32) `shouldBe` True
      it "Says that Int# 2200000000 is invalid" $ isValid (I32# 2200000000#) `shouldBe` False
      it "Says that Int# -2200000000 is invalid" $ isValid (I32# (-2200000000#)) `shouldBe` False
    describe "Validity Word8" $ do
      it "Says that minBound is valid" $ isValid (minBound :: Word8) `shouldBe` True
      it "Says that maxBound is valid" $ isValid (maxBound :: Word8) `shouldBe` True
      it "Says that Word# 300 is invalid" $ isValid (W8# 300##) `shouldBe` False
    describe "Validity Word16" $ do
      it "Says that minBound is valid" $ isValid (minBound :: Word16) `shouldBe` True
      it "Says that maxBound is valid" $ isValid (maxBound :: Word16) `shouldBe` True
      it "Says that Word# 80000 is invalid" $ isValid (W16# 80000##) `shouldBe` False
    describe "Validity Word32" $ do
      it "Says that minBound is valid" $ isValid (minBound :: Word32) `shouldBe` True
      it "Says that maxBound is valid" $ isValid (maxBound :: Word32) `shouldBe` True
      it "Says that Word# 4800000000 is invalid" $ isValid (W32# 4800000000##) `shouldBe` False
  describe "Chars" $ do
    describe "Small" $ do
      describe "Validity Char" $ do
        it "Says that minBound is valid" $ isValid (minBound :: Char) `shouldBe` True
        it "Says that maxBound is valid" $ isValid (maxBound :: Char) `shouldBe` True
        it "Says that 2147483647 is invalid" $ isValid (C# (chr# 2147483647#)) `shouldBe` False
        it "Says that a negative char is invalid" $ isValid (C# (chr# -1#)) `shouldBe` False
        it "Says that a very positive char is invalid" $ isValid (C# (chr# 9223372036854775807#)) `shouldBe` False
        it "Says that a very negative char is invalid" $ isValid (C# (chr# -9223372036854775808#)) `shouldBe` False
    describe "Weird" $ do
      describe "isUtf16SurrogateCodePoint" $ do
        it "Says that a is a valid char" $ isUtf16SurrogateCodePoint 'a' `shouldBe` False
        it "Says that \\55810 is an invalid char" $ isUtf16SurrogateCodePoint '\55810' `shouldBe` True
      describe "validateCharNotUtf16SurrogateCodePoint" $ do
        it "Says that a is a valid char" $
          prettyValidation (validateCharNotUtf16SurrogateCodePoint 'a') `shouldSatisfy` isNothing
        it "Says that \\55810 is an invalid char" $
          prettyValidation (validateCharNotUtf16SurrogateCodePoint '\55810') `shouldSatisfy` isJust
    describe "Lines" $ do
      describe "isLineSeparator" $ do
        it "Says that a is not a line separator" $ isLineSeparator 'a' `shouldBe` False
        it "Says that '\\n' is a line separator " $ isLineSeparator '\n' `shouldBe` True
        it "Says that '\\r' is a line separator " $ isLineSeparator '\r' `shouldBe` True
      describe "validateCharNotLineSeparator" $ do
        it "Says that a is not a line separator" $
          prettyValidation (validateCharNotLineSeparator 'a') `shouldSatisfy` isNothing
        it "Says that '\\n' is a line separator" $
          prettyValidation (validateCharNotLineSeparator '\n') `shouldSatisfy` isJust
        it "Says that '\\r' is a line separator" $
          prettyValidation (validateCharNotLineSeparator '\r') `shouldSatisfy` isJust
      describe "isSingleLine" $ do
        it "says that \"abc\" is a single line" $ isSingleLine "abc" `shouldBe` True
        it "says that \"d\ne\" is a single line" $ isSingleLine "d\ne" `shouldBe` False
      describe "validateStringSingleLine" $ do
        it "says that \"abc\" is a single line" $ prettyValidation (validateStringSingleLine "abc") `shouldSatisfy` isNothing
        it "says that \"d\ne\" is a single line" $ prettyValidation (validateStringSingleLine "d\ne") `shouldSatisfy` isJust
  describe "Ratio" $ do
    it "says that 0 is valid" $ NormalisedRatio (0 :% 1 :: Ratio Int) `shouldSatisfy` isValid
    it "says that 1 is valid" $ NormalisedRatio (1 :% 1 :: Ratio Int) `shouldSatisfy` isValid
    it "says that minBound is valid" $
      NormalisedRatio (minBound :% 1 :: Ratio Int) `shouldSatisfy` isValid
    it "says that maxBound is valid" $
      NormalisedRatio (maxBound :% 1 :: Ratio Int) `shouldSatisfy` isValid
    it "says that maxBound / minBound is invalid" $
      NormalisedRatio (maxBound :% minBound :: Ratio Int) `shouldSatisfy` (not . isValid)
    it "says that minBound / maxBound is invalid" $
      NormalisedRatio (minBound :% maxBound :: Ratio Int) `shouldSatisfy` (not . isValid)
    it "says that minBound / 2957808295740799111 is valid" $
      NormalisedRatio (minBound :% (2957808295740799111) :: Ratio Int) `shouldSatisfy` isValid
  describe "NormalisedRatio" $ do
    it "says that NaN is invalid" $ NormalisedRatio notANumber `shouldSatisfy` (not . isValid)
    it "says that +Inf is invalid" $ NormalisedRatio infinity `shouldSatisfy` (not . isValid)
    it "says that -Inf is invalid" $ NormalisedRatio (- infinity) `shouldSatisfy` (not . isValid)
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
