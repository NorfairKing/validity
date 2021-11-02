{-# LANGUAGE TypeApplications #-}

module Test.Syd.Validity.ShrinkingSpec where

import Data.Int
import Data.Ratio
import Test.Syd
import Test.Syd.Validity.Shrinking

spec :: Spec
spec = do
  shrinkValidSpec @(Ratio Int8)
  shrinkValidSpec @Int
  describe "shrinkValidPreservesValidOnGenValid" $ do
    it "Ordering" $ shrinkValidPreservesValidOnGenValid @Ordering
    it "[Ordering]" $ shrinkValidPreservesValidOnGenValid @[Ordering]
  describe "shrinkValidPreservesValidOnGenValid" $ do
    it "Ordering" $ shrinkValidPreservesValidOnGenValid @Ordering
    it "[Ordering]" $ shrinkValidPreservesValidOnGenValid @[Ordering]
  describe "shrinkValidDoesNotShrinkToItself" $ do
    it "Int" $ shrinkValidDoesNotShrinkToItself @Int
    it "[Int]" $ shrinkValidDoesNotShrinkToItself @[Int]
  describe "shrinkValidDoesNotShrinkToItself" $ do
    it "Ordering" $ shrinkValidDoesNotShrinkToItself @Ordering
    it "[Ordering]" $ shrinkValidDoesNotShrinkToItself @[Ordering]
