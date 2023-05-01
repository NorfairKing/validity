{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SydCheckSpec (spec) where

import qualified Data.Vector.Unboxed as UV
import Data.Word
import SydCheck
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
    goldenGenValidSpec @(Maybe Word8) "maybe-word8"
    goldenGenValidSpec @[Word8] "list-word8"
    goldenGenValidSpec @[[Word8]] "list-list-word8"
    goldenGenValidSpec @Word64 "word64"
    goldenGenSpec (genInt (0, 100)) "percentage"
    goldenGenSpec genProperFraction "proper-fraction"
    goldenGenSpec (genPartition 100) "partition-100"

  describe "generator tests" $ do
    describe "genInt" $
      it "generates values in the given range" $
        let lo = 6
            hi = 17
         in generatorProperty (genInt (lo, hi)) (\a -> lo <= a && a <= hi)
    describe "genDouble" $
      it "generates values in the given range" $
        let lo = 6
            hi = 17
         in generatorProperty (genDouble (lo, hi)) (\a -> lo <= a && a <= hi)

    describe "genProperFraction" $
      it "generates values in the range [0,1]" $
        generatorProperty genProperFraction (\a -> 0 <= a && a <= 1)

    describe "genPartition" $
      it "generates values that sum up to the original value" $
        generatorProperty
          (genPartition 1000)
          ( \ls ->
              let total = sum ls
                  expected = 1000
                  diff = abs $ total - expected
               in diff < 50
          )

    describe "suchThat" $ do
      it "generates values that satisfy the predicate" $
        generatorProperty
          (genInt (0, 1) `suchThat` (>= 1))
          (>= 1)

  describe "runIsProperty" $ do
    let -- TODO multiple acceptable counterexamples
        findsCounterExampleSpec ::
          (Show (PList ls), Eq (PList ls), IsProperty ls prop) =>
          prop ->
          PList ls ->
          IO ()
        findsCounterExampleSpec property counterexample =
          runIsProperty 100 1000 100000 100 42 property
            `shouldBe` Right (Just counterexample)
        doesNotFindCounterExampleSpec ::
          forall ls prop.
          (Show (PList ls), Eq (PList ls), IsProperty ls prop) =>
          prop ->
          IO ()
        doesNotFindCounterExampleSpec property =
          runIsProperty @ls 100 1000 100000 100 42 property
            `shouldBe` Right Nothing
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

generatorProperty :: forall a. (Show a, Eq a) => Gen a -> (a -> Bool) -> IO ()
generatorProperty gen predicate = do
  let p = forAll gen predicate
  runIsProperty 100 1000 0 100 42 p `shouldBe` Right Nothing

goldenGenValidSpec :: forall a. (Show a, GenValid a) => FilePath -> Spec
goldenGenValidSpec = goldenGenSpec (genValid @a)

goldenGenSpec :: forall a. (Show a) => Gen a -> FilePath -> Spec
goldenGenSpec gen fp = do
  let nbValues = 100
      maxSize = nbValues
      sizes = [0 .. maxSize]
      seeds = [42 ..]
  it ("generates the same " <> fp <> " values") $ do
    let randomnesses = zipWith computeRandomness sizes seeds
        values :: [Either String a]
        values = map (runGen gen) randomnesses
    pureGoldenStringFile ("test_resources/gen/" <> fp <> ".txt") $
      unlines $
        map (either id show) values
  it ("shrinks to the same " <> fp <> " values") $ do
    let (randomness, maxValue) = runGenUntilSucceeds maxSize 42 gen
        shrinks :: [Either String a]
        shrinks = map (runGen gen) $ take 10 $ computeAllShrinks randomness
    pureGoldenStringFile ("test_resources/shrink/" <> fp <> ".txt") $
      unlines $
        show maxValue
          : ""
          : map (either id show) shrinks
