{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SydCheck.GenSpec (spec, goldenGenSpec, genDefinitelyGenerates) where

import SydCheck
import SydCheck.Gen
import SydCheck.Property
import SydCheck.Randomness
import SydCheck.Runner
import SydCheck.Shrinking
import Test.Syd

spec :: Spec
spec = do
  describe "runGen" $ do
    goldenGenSpec (genInt (0, 100)) "percentage-int"
    goldenGenSpec (genWord (0, 100)) "percentage-word"
    goldenGenSpec (genChar ('a', 'z')) "alphabet"
    goldenGenSpec genProperFraction "proper-fraction"
    goldenGenSpec (genPartition 100) "partition-100"
    goldenGenSpec (genMaybeOf (genWord (0, 100))) "maybe-percentage-word"
    goldenGenSpec (genEitherOf (genWord (0, 100)) (genWord (0, 100))) "either-percentage-percentage"
    goldenGenSpec (genListOf (genWord (0, 100))) "list-percentage-word"
    genDefinitelyGenerates (genListOf (genWord (0, 100))) []
    goldenGenSpec (genNonEmptyOf (genWord (0, 100))) "nonempty-percentage-word"

  describe "generator tests" $ do
    describe "genInt" $
      it "generates values in the given range" $
        let lo = 6
            hi = 17
         in generatorProperty
              (genInt (lo, hi))
              (\a -> lo <= a && a <= hi)
    describe "genWord" $ do
      it "generates values in the given range" $
        let lo = 7
            hi = 18
         in generatorProperty (genWord (lo, hi)) (\a -> lo <= a && a <= hi)
    describe "genDouble" $
      it "generates values in the given range" $
        let lo = 8
            hi = 19
         in generatorProperty (genDouble (lo, hi)) (\a -> lo <= a && a <= hi)

    describe "genProperFraction" $
      it "generates values in the range [0,1]" $
        generatorProperty genProperFraction (\a -> 0 <= a && a <= 1)

    describe "suchThat" $ do
      it "generates values that satisfy the predicate" $
        generatorProperty
          (genInt (0, 1) `suchThat` (>= 1))
          (>= 1)

generatorProperty :: forall a. (Show a, Eq a) => Gen a -> (a -> Bool) -> IO ()
generatorProperty gen predicate = do
  let p = forAll gen predicate :: TypedPropertyT '[a] IO
  result <- runTypedPropertyT 100 1000 0 100 (Just 42) p
  case result of
    ResultNoCounterexample -> pure ()
    _ -> expectationFailure $ show result

goldenGenSpec :: forall a. (Show a) => Gen a -> FilePath -> Spec
goldenGenSpec gen fp = do
  let nbValues = 100
      maxSize = nbValues
      sizes = [0 .. maxSize]
      initialSeed = 42
      seeds = [initialSeed ..]
  it ("generates the same " <> fp <> " values") $ do
    let randomnesses = zipWith computeRandomness sizes seeds
        values :: [Either String a]
        values = map (runGen gen) randomnesses
    pureGoldenStringFile ("test_resources/gen/" <> fp <> ".txt") $
      unlines $
        map (either id show) values
  it ("shrinks to the same " <> fp <> " values") $ do
    let (randomness, maxValue) = runGenUntilSucceeds maxSize initialSeed gen
        shrinks :: [Either String a]
        shrinks = map (runGen gen) $ take 10 $ computeAllShrinks randomness
    pureGoldenStringFile ("test_resources/shrink/" <> fp <> ".txt") $
      unlines $
        show maxValue
          : ""
          : map (either id show) shrinks

genDefinitelyGenerates :: (Show a, Eq a) => Gen a -> a -> Spec
genDefinitelyGenerates gen value = do
  let nbValues = 100
      maxSize = nbValues
      sizes = [0 .. maxSize]
      initialSeed = 42
      seeds = [initialSeed ..]
  it ("definitely generates " <> show value) $ do
    let randomnesses = zipWith computeRandomness sizes seeds
        values = map (runGen gen) randomnesses
    shouldSatisfyNamed values ("contains " <> show value) (elem (Right value))
