{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GenSpec (spec) where

import qualified Data.Vector.Unboxed as UV
import Data.Word
import Gen
import Test.Syd

spec :: Spec
spec = do
  describe "computeSplit" $
    it "shrinks to 0" $
      computeSplit 30 0 `shouldBe` 0

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

  describe "computeSizes" $ do
    it "returns [0..n] when successes is one more than maxSize" $
      computeSizes 11 10 `shouldBe` [0 .. 10]

  describe "runGen" $ do
    goldenGenValidSpec @Bool "bool"
    goldenGenValidSpec @Word8 "word8"
    goldenGenValidSpec @(Word8, Word8) "tuple-word8-word8"
    goldenGenValidSpec @[Word8] "list-word8"
    goldenGenValidSpec @Word64 "word64"
    goldenGenSpec (genInt (0, 100)) "percentage"
    goldenGenSpec genProperFraction "proper-fraction"

  describe "runIsProperty" $ do
    let findsCounterExampleSpec ::
          (Show (PList ls), Eq (PList ls), IsProperty ls prop) =>
          prop ->
          PList ls ->
          IO ()
        findsCounterExampleSpec property counterexample =
          runIsProperty 100 1000 100000 42 property
            `shouldBe` Just counterexample
        doesNotFindCounterExampleSpec ::
          forall ls prop.
          (Show (PList ls), Eq (PList ls), IsProperty ls prop) =>
          prop ->
          IO ()
        doesNotFindCounterExampleSpec property =
          runIsProperty @ls 100 1000 100000 42 property
            `shouldBe` Nothing
    it "finds a counterexample for False" $
      findsCounterExampleSpec False PNil
    it "finds no counterexample for True" $
      doesNotFindCounterExampleSpec True
    it "finds a counterexample for id" $
      findsCounterExampleSpec (\b -> (b :: Bool)) (PCons False PNil)
    it "finds no counterexample for const True" $
      doesNotFindCounterExampleSpec (\(_ :: Bool) -> True)
    it "finds a counterexample for w < 2" $
      findsCounterExampleSpec
        (\w -> w < (2 :: Word8))
        (PCons (2 :: Word8) PNil)
    it "finds a counterexample for w1 >= w2" $
      findsCounterExampleSpec
        (\w1 w2 -> w1 >= (w2 :: Word8))
        (PCons (0 :: Word8) (PCons (1 :: Word8) PNil))
    it "finds a counterexample for w1 <= w2" $
      findsCounterExampleSpec
        (\w1 w2 -> w1 <= (w2 :: Word8))
        (PCons (1 :: Word8) (PCons (0 :: Word8) PNil))
    it "finds a counterexample for reverse not (null ls)" $
      findsCounterExampleSpec
        (\ls -> not (null (ls :: [Word8])))
        (PCons ([] :: [Word8]) PNil)
    it "does not find a counter example for ls <= ls" $
      doesNotFindCounterExampleSpec
        (\ls -> ls <= (ls :: [Word8]))
    it "finds a counterexample for reverse ls == ls" $
      findsCounterExampleSpec
        (\ls -> reverse ls == (ls :: [Word8]))
        (PCons ([0, 1] :: [Word8]) PNil)
    it "finds a counterexample for reverse ls < ls" $
      findsCounterExampleSpec
        (\ls -> reverse ls < (ls :: [Word8]))
        (PCons ([] :: [Word8]) PNil)

goldenGenValidSpec :: forall a. (Show a, GenValid a) => FilePath -> Spec
goldenGenValidSpec = goldenGenSpec (genValid @a)

goldenGenSpec :: forall a. (Show a) => Gen a -> FilePath -> Spec
goldenGenSpec gen fp = do
  let nbValues = 100
      sizes = [0 .. nbValues]
      seeds = [42 ..]
  it ("generates the same " <> fp <> " values") $ do
    let randomnesses = zipWith computeRandomness sizes seeds
        values :: [a]
        values = map (runGen gen) randomnesses
    pureGoldenStringFile ("test_resources/gen/" <> fp <> ".txt") $
      unlines $
        map show values
  it ("shrinks to the same " <> fp <> " values") $ do
    let randomness = computeRandomness nbValues 42
        maxValue :: a
        maxValue = runGen gen randomness
        shrinks :: [a]
        shrinks = map (runGen gen) $ take 10 $ computeAllShrinks randomness
    pureGoldenStringFile ("test_resources/shrink/" <> fp <> ".txt") $
      unlines $
        show maxValue
          : ""
          : map show shrinks
