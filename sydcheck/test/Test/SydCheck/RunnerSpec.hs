{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.SydCheck.RunnerSpec (spec) where

import Data.Validity
import Data.Word
import GHC.Generics (Generic)
import Test.Syd
import Test.SydCheck
import Test.SydCheck.GenValid
import Test.SydCheck.PList
import Test.SydCheck.Property
import Test.SydCheck.Runner

spec :: Spec
spec = do
  describe "computeSizes" $ do
    it "returns [0..n] when successes is one more than maxSize" $
      computeSizes 11 10 `shouldBe` [0 .. 10]

  describe "runIsProperty" $ do
    let -- TODO multiple acceptable counterexamples
        findsCounterExampleSpec ::
          forall ls prop.
          (Show (PList ls), Eq (PList ls), IsTypedPropertyT ls IO prop) =>
          prop ->
          PList ls ->
          IO ()
        findsCounterExampleSpec property counterexample = do
          result <- runIsTypedPropertyT @ls 100 1000 10000 100 (Just 42) property
          case result of
            ResultCounterexample values _ _ _ -> values `shouldBe` counterexample
            _ -> expectationFailure $ show result
        doesNotFindCounterExampleSpec ::
          forall ls prop.
          (Show (PList ls), Eq (PList ls), IsTypedPropertyT ls IO prop) =>
          prop ->
          IO ()
        doesNotFindCounterExampleSpec property = do
          result <- runIsTypedPropertyT @ls 100 1000 10000 100 (Just 42) property
          case result of
            ResultNoCounterexample -> pure ()
            _ -> expectationFailure $ show result
    it "finds a counterexample for False" $
      findsCounterExampleSpec False PNil
    it "finds no counterexample for True" $
      doesNotFindCounterExampleSpec @'[] True
    it "finds a counterexample for id" $
      findsCounterExampleSpec (\b -> (b :: Bool)) (PCons False PNil)
    it "finds no counterexample for const True" $
      doesNotFindCounterExampleSpec @'[Bool] (\(_ :: Bool) -> True)
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
      doesNotFindCounterExampleSpec @'[[Word8]]
        (\ls -> ls <= (ls :: [Word8]))
    it "finds a counterexample for reverse ls == ls" $
      findsCounterExampleSpec
        (\ls -> reverse ls == (ls :: [Word8]))
        (PCons ([0, 1] :: [Word8]) PNil)
    it "finds a counterexample for reverse ls < ls" $
      findsCounterExampleSpec
        (\ls -> reverse ls < (ls :: [Word8]))
        (PCons ([] :: [Word8]) PNil)
    it "finds a counterexample for 'all even numbers are smaller than 3'" $
      findsCounterExampleSpec
        (forAll ((* 2) <$> genValid) (\i -> i < (3 :: Int)) :: TypedPropertyT '[Int] IO ())
        (PCons (4 :: Int) PNil :: PList '[Int])
    it "finds a counterexample in this generic structure" $
      findsCounterExampleSpec
        exampleBool
        (PCons (Example {exampleBool = False, exampleInt = 0}) PNil)
    it "finds a counterexample in this generic structure" $
      findsCounterExampleSpec
        (\example -> not (exampleBool example) || even (exampleInt example))
        (PCons (Example {exampleBool = True, exampleInt = 1}) PNil)

data Example = Example
  { exampleBool :: Bool,
    exampleInt :: Int
  }
  deriving (Show, Eq, Generic)

instance Validity Example

instance GenValid Example
