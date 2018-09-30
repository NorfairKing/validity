{-# LANGUAGE TypeApplications #-}

module Test.Validity.ShrinkingSpec where

import Test.Hspec

import Test.Validity.Shrinking

spec :: Spec
spec = do
    shrinkValiditySpec @Double
    shrinkValidSpec @Int
    shrinkInvalidSpec @Double
    describe "shrinkUncheckedPreservesValidOnGenValid" $ do
        it "Double" $ shrinkValidPreservesValidOnGenValid @Double
        it "[Double]" $ shrinkValidPreservesValidOnGenValid @[Double]
    describe "shrinkValidPreservesValidOnGenValid" $ do
        it "Double" $ shrinkValidPreservesValidOnGenValid @Double
        it "[Double]" $ shrinkValidPreservesValidOnGenValid @[Double]
    describe "shrinkInvalidPreservesInvalidOnGenInvalid" $ do
        it "Double" $ shrinkInvalidPreservesInvalidOnGenInvalid @Double
        it "[Double]" $ shrinkInvalidPreservesInvalidOnGenInvalid @[Double]
    describe "shrinkUncheckedDoesNotShrinkToItself" $ do
        it "Int" $ shrinkUncheckedDoesNotShrinkToItself @Int
        it "[Int]" $ shrinkUncheckedDoesNotShrinkToItself @[Int]
    describe "shrinkValidDoesNotShrinkToItself" $ do
        it "Double" $ shrinkValidDoesNotShrinkToItself @Double
        it "[Double]" $ shrinkValidDoesNotShrinkToItself @[Double]
