{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GenSpec (spec) where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as UV
import Data.Word
import Gen
import Test.Syd

spec :: Spec
spec = do
  describe "computeSplit" $
    it "shrinks to 0" $
      computeSplit 30 0 `shouldBe` 0

  describe "computePartition" $ do
    it "shrinks to [] for a zero size" $
      computePartition 0 0 `shouldBe` []
    it "shrinks to [size] for a nonzero size" $
      computePartition 30 0 `shouldBe` [30]

  describe "shrinkRandomness" $
    it "does not shrink an empty vector" $
      shrinkRandomness UV.empty `shouldBe` []

  describe "shrinkSingleWord64" $
    it "does not shrink 0" $
      shrinkSingleWord64 0 `shouldBe` []

  describe "runGen" $ do
    goldenGenSpec @Bool "bool"
    goldenGenSpec @Word8 "word8"
    goldenGenSpec @(Word8, Word8) "tuple-word8-word8"
    goldenGenSpec @[Word8] "tuple-word8-word8"
    goldenGenSpec @Word64 "word64"

goldenGenSpec :: forall a. (Show a, GenValid a) => FilePath -> Spec
goldenGenSpec fp = do
  let nbValues = 100
      sizes = [0 .. nbValues]
      seeds = [42 ..]
  it "generates the same values" $ do
    let randomnesses = zipWith computeRandomness sizes seeds
        values :: [a]
        values = map (runGen genValid) randomnesses
    pureGoldenStringFile ("test_resources/gen/" <> fp <> ".txt") $
      unlines $
        map show values
  it "shrinks to the same Word64 values" $ do
    let randomness = computeRandomness nbValues 42
        maxValue :: a
        maxValue = runGen genValid randomness
        shrinks :: [a]
        shrinks = map (runGen genValid) $ take 100 $ computeAllShrinks randomness
    pureGoldenStringFile ("test_resources/shrink/" <> fp <> ".txt") $
      unlines $
        show maxValue
          : ""
          : map show shrinks
