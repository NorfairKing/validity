{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SydCheck.GenSpec (spec) where

import SydCheck
import SydCheck.Gen
import SydCheck.Shrinking
import Test.Syd

spec :: Spec
spec = do
  describe "computeSplit" $
    it "shrinks to 0" $
      computeSplit 30 0 `shouldBe` 0

  describe "runGen" $ do
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

generatorProperty :: forall a. (Show a, Eq a) => Gen a -> (a -> Bool) -> IO ()
generatorProperty gen predicate = do
  let p = forAll gen predicate
  runIsProperty 100 1000 0 100 42 p `shouldBe` Right Nothing

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
