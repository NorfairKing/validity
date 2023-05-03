{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SydCheckSpec (spec) where

import Data.Word
import SydCheck
import SydCheck.PList
import SydCheck.Property
import Test.Syd

spec :: Spec
spec = do
  describe "computeSizes" $ do
    it "returns [0..n] when successes is one more than maxSize" $
      computeSizes 11 10 `shouldBe` [0 .. 10]

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
