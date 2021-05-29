module Data.GenValiditySpec
  ( spec,
  )
where

import Data.GenValidity
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "genUtf16SurrogateCodePoint" $
    it "generates Utf16 surrogate codepoints" $
      forAll genUtf16SurrogateCodePoint (`shouldSatisfy` isUtf16SurrogateCodePoint)
  describe "genLineSeparator" $
    it "generates only line separators" $
      forAll genLineSeparator (`shouldSatisfy` isLineSeparator)
  describe "genNonLineSeparator" $
    it "never generates line separators" $
      forAll genNonLineSeparator (`shouldSatisfy` (not . isLineSeparator))
  describe "genSingleLineString" $
    it "generates only single line strings" $
      forAll genSingleLineString (`shouldSatisfy` isSingleLine)
  describe "upTo" $ do
    it "returns only positive integers" $
      forAll arbitrary $ \n -> forAll (upTo n) (`shouldSatisfy` (>= 0))
    it "returns only integers smaller than or equal to the given number" $
      forAll arbitrary $ \n ->
        forAll (upTo n) (`shouldSatisfy` (<= (max n 0)))
  describe "genSplit" $ do
    it "returns positive integers" $
      forAll arbitrary $ \i ->
        forAll (genSplit i) $ \(a, b) -> do
          a `shouldSatisfy` (>= 0)
          b `shouldSatisfy` (>= 0)
    it "returns two integers such that the sum is the original integer" $
      forAll arbitrary $ \i ->
        forAll (genSplit i) $ \(a, b) -> a + b `shouldBe` max 0 i
  describe "genSplit3" $ do
    it "returns positive integers" $
      forAll arbitrary $ \i ->
        forAll (genSplit3 i) $ \(a, b, c) -> do
          a `shouldSatisfy` (>= 0)
          b `shouldSatisfy` (>= 0)
          c `shouldSatisfy` (>= 0)
    it "returns three integers such that the sum is the original integer" $
      forAll arbitrary $ \i ->
        forAll (genSplit3 i) $ \(a, b, c) ->
          a + b + c `shouldBe` max 0 i
  describe "genSplit4" $ do
    it "returns positive integers" $
      forAll arbitrary $ \i ->
        forAll (genSplit4 i) $ \(a, b, c, d) -> do
          a `shouldSatisfy` (>= 0)
          b `shouldSatisfy` (>= 0)
          c `shouldSatisfy` (>= 0)
          d `shouldSatisfy` (>= 0)
    it "returns four integers such that the sum is the original integer" $
      forAll arbitrary $ \i ->
        forAll (genSplit4 i) $ \(a, b, c, d) ->
          a + b + c + d `shouldBe` max 0 i
  describe "genSplit5" $ do
    it "returns positive integers" $
      forAll arbitrary $ \i ->
        forAll (genSplit5 i) $ \(a, b, c, d, e) -> do
          a `shouldSatisfy` (>= 0)
          b `shouldSatisfy` (>= 0)
          c `shouldSatisfy` (>= 0)
          d `shouldSatisfy` (>= 0)
          e `shouldSatisfy` (>= 0)
    it "returns four integers such that the sum is the original integer" $
      forAll arbitrary $ \i ->
        forAll (genSplit5 i) $ \(a, b, c, d, e) ->
          a + b + c + d + e `shouldBe` max 0 i
  describe "arbPartition" $ do
    it "returns an empty list upon strictly negative input" $
      forAll (arbitrary `suchThat` (< 0)) $ \n ->
        forAll (arbPartition n) (`shouldBe` [])
    it "returns a list of positive integers" $
      forAll arbitrary $ \n ->
        forAll (arbPartition n) $ \p -> p `shouldSatisfy` all (>= 0)
