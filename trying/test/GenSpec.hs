{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
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

  xdescribe "computePartition" $ do
    it "shrinks to [] for a zero size" $
      computePartition 0 0 `shouldBe` []
    it "shrinks to [size] for a nonzero size" $
      computePartition 30 0 `shouldBe` [30]

  describe "shrinkRandomness" $ do
    it "does not shrink an empty vector" $
      shrinkRandomness UV.empty
        `shouldBe` []
    it "always tries to shrink to a smaller vector from the beginning" $
      let shrinks = shrinkRandomness $ UV.fromList [1, 2, 3]
       in shrinks `shouldSatisfy` (UV.fromList [2, 3] `elem`)
    it "always tries to shrink to a smaller vector from the end" $
      let shrinks = shrinkRandomness $ UV.fromList [1, 2, 3]
       in shrinks `shouldSatisfy` (UV.fromList [1, 2] `elem`)

  describe "runProperty" $ do
    shrinksToSpec
      -- \w -> w < 1
      (PropGen genValid (\w -> PropBool ((w :: Word8) < 1)))
      (PCons 1 PNil)
    shrinksToSpec
      -- \w1 -> w2 -> w1 + w2 < 2
      (PropGen genValid (\w1 -> (PropGen genValid (\w2 -> PropBool ((w1 + w2 :: Word8) < 2)))))
      (PCons 1 (PCons 1 PNil))
    shrinksToSpec
      -- \w1 -> w2 -> w1 >= w2
      (PropGen genValid (\w1 -> (PropGen genValid (\w2 -> PropBool (w1 >= (w2 :: Word8))))))
      (PCons 0 (PCons 1 PNil))

  describe "runGen" $ do
    goldenGenSpec @Bool "bool"
    goldenGenSpec @Word8 "word8"
    goldenGenSpec @(Word8, Word8) "tuple-word8-word8"
    -- goldenGenSpec @[Word8] "tuple-word8-word8"
    goldenGenSpec @Word64 "word64"

shrinksToSpec ::
  (Show (PList ls), Eq (PList ls)) =>
  Property ls ->
  PList ls ->
  Spec
shrinksToSpec property shrunk = do
  let randomness = computeRandomness 100 32
  it "fails the property" $
    let (_, result) = runProperty randomness property
     in result `shouldBe` False

  it ("shrinks to " <> show shrunk) $
    case shrinkProperty randomness property of
      Nothing -> expectationFailure "should have been able to shrink"
      Just values -> values `shouldBe` shrunk

goldenGenSpec :: forall a. (Show a, GenValid a) => FilePath -> Spec
goldenGenSpec fp = do
  let nbValues = 100
      sizes = [0 .. nbValues]
      seeds = [42 ..]
  it ("generates the same " <> fp <> " values") $ do
    let randomnesses = zipWith computeRandomness sizes seeds
        values :: [a]
        values = map (runGen genValid) randomnesses
    pureGoldenStringFile ("test_resources/gen/" <> fp <> ".txt") $
      unlines $
        map show values
  it ("shrinks to the same " <> fp <> " values") $ do
    let randomness = computeRandomness nbValues 42
        maxValue :: a
        maxValue = runGen genValid randomness
        shrinks :: [a]
        shrinks = map (runGen genValid) $ take 10 $ computeAllShrinks randomness
    pureGoldenStringFile ("test_resources/shrink/" <> fp <> ".txt") $
      unlines $
        show maxValue
          : ""
          : map show shrinks
