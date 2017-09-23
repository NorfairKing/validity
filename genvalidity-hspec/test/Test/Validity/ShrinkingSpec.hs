{-# LANGUAGE TypeApplications #-}

module Test.Validity.ShrinkingSpec where

import Test.Hspec

import Test.Validity.Shrinking

spec :: Spec
spec = do
    shrinkValiditySpec @Double
    shrinkValidSpec @Int
    shrinkInvalidSpec @Double
    describe "shrinkValidPreservesValidOnGenValid" $ do
        it "Int" $ shrinkValidPreservesValidOnGenValid @Int
        it "Double" $ shrinkValidPreservesValidOnGenValid @Double
        it "[Double]" $ shrinkValidPreservesValidOnGenValid @[Double]
    describe "shrinkInvalidPreservesInvalidOnGenInvalid" $ do
        it "Double" $ shrinkInvalidPreservesInvalidOnGenInvalid @Double
        it "[Double]" $ shrinkInvalidPreservesInvalidOnGenInvalid @[Double]
