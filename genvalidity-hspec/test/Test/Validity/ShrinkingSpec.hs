{-# LANGUAGE TypeApplications #-}

module Test.Validity.ShrinkingSpec where

import Test.Hspec

import Data.Int
import Data.Ratio

import Test.Validity.Shrinking

spec :: Spec
spec = do
  shrinkValiditySpec @(Ratio Int8)
  shrinkValidSpec @Int
  shrinkInvalidSpec @(Ratio Int8)
  describe "shrinkUncheckedPreservesValidOnGenValid" $ do
    it "Ordering" $ shrinkValidPreservesValidOnGenValid @Ordering
    it "[Ordering]" $ shrinkValidPreservesValidOnGenValid @[Ordering]
  describe "shrinkValidPreservesValidOnGenValid" $ do
    it "Ordering" $ shrinkValidPreservesValidOnGenValid @Ordering
    it "[Ordering]" $ shrinkValidPreservesValidOnGenValid @[Ordering]
  describe "shrinkInvalidPreservesInvalidOnGenInvalid" $ do
    it "Ordering" $ shrinkInvalidPreservesInvalidOnGenInvalid @(Ratio Int8)
    it "[Ordering]" $ shrinkInvalidPreservesInvalidOnGenInvalid @[Ratio Int8]
  describe "shrinkUncheckedDoesNotShrinkToItself" $ do
    it "Int" $ shrinkUncheckedDoesNotShrinkToItself @Int
    it "[Int]" $ shrinkUncheckedDoesNotShrinkToItself @[Int]
  describe "shrinkValidDoesNotShrinkToItself" $ do
    it "Ordering" $ shrinkValidDoesNotShrinkToItself @Ordering
    it "[Ordering]" $ shrinkValidDoesNotShrinkToItself @[Ordering]
