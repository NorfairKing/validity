{-# LANGUAGE TypeApplications #-}

module Test.Validity.ShrinkingSpec where

import Test.Hspec

import Test.Validity.Shrinking

spec :: Spec
spec = do
    shrinkValiditySpec @Rational
    shrinkValidSpec @Int
    shrinkInvalidSpec @Rational
    describe "shrinkUncheckedPreservesValidOnGenValid" $ do
        it "Rational" $ shrinkValidPreservesValidOnGenValid @Rational
        it "[Rational]" $ shrinkValidPreservesValidOnGenValid @[Rational]
    describe "shrinkValidPreservesValidOnGenValid" $ do
        it "Rational" $ shrinkValidPreservesValidOnGenValid @Rational
        it "[Rational]" $ shrinkValidPreservesValidOnGenValid @[Rational]
    describe "shrinkInvalidPreservesInvalidOnGenInvalid" $ do
        it "Rational" $ shrinkInvalidPreservesInvalidOnGenInvalid @Rational
        it "[Rational]" $ shrinkInvalidPreservesInvalidOnGenInvalid @[Rational]
    describe "shrinkUncheckedDoesNotShrinkToItself" $ do
        it "Int" $ shrinkUncheckedDoesNotShrinkToItself @Int
        it "[Int]" $ shrinkUncheckedDoesNotShrinkToItself @[Int]
    describe "shrinkValidDoesNotShrinkToItself" $ do
        it "Rational" $ shrinkValidDoesNotShrinkToItself @Rational
        it "[Rational]" $ shrinkValidDoesNotShrinkToItself @[Rational]
