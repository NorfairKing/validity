{-# LANGUAGE DeriveGeneric #-}

module Data.GenValidity.ShrinkGenericSpec where

import GHC.Generics (Generic)

import Test.Hspec

import Data.GenValidity

spec :: Spec
spec = do
    describe "genericShrinkUnchecked" $ do
        it "shrinks tuples correctly" $
            genericShrinkUnchecked ((A2, B3)) `shouldBe`
            [(A1, B1), (A1, B2), (A1, B3), (A2, B1), (A2, B2)]
        it "figures out the right shrinking function for Ex" $
            genericShrinkUnchecked (Ex A2 B3) `shouldBe`
            [Ex A1 B1, Ex A1 B2, Ex A1 B3, Ex A2 B1, Ex A2 B2]
    describe "default shrinkValid" $ do
        it "figures out the right shrinking function for A" $
            shrinkValid A2 `shouldBe` [A1]
        it "figures out the right shrinking function for B" $
            shrinkValid B3 `shouldBe` [B1]
        it "shrinks tuples correctly" $
            shrinkValid ((A2, B3)) `shouldBe` [(A1, B1), (A1, B3), (A2, B1)]
        it "figures out the right shrinking function for Ex" $
            shrinkValid (Ex A2 B3) `shouldBe` [Ex A1 B1, Ex A1 B3, Ex A2 B1]
    describe "shrinkValidStructurally" $ do
        it "shrinks tuples correctly" $
            shrinkValidStructurally ((A2, B3)) `shouldBe`
            [(A1, B1), (A1, B3), (A2, B1)]
        it "figures out the right shrinking function for Ex" $
            shrinkValidStructurally (Ex A2 B3) `shouldBe`
            [Ex A1 B1, Ex A1 B3, Ex A2 B1]

data Ex =
    Ex A
       B
    deriving (Show, Eq, Generic)

instance Validity Ex

instance GenUnchecked Ex

instance GenValid Ex

data A
    = A1
    | A2
    deriving (Show, Eq, Generic)

instance Validity A

instance GenUnchecked A where
    shrinkUnchecked A1 = []
    shrinkUnchecked A2 = [A1]

instance GenValid A

data B
    = B1
    | B2
    | B3
    deriving (Show, Eq, Generic)

instance Validity B where
    validate B1 = valid
    validate B2 = invalid "for test"
    validate B3 = valid

instance GenUnchecked B where
    shrinkUnchecked B1 = []
    shrinkUnchecked B2 = [B1]
    shrinkUnchecked B3 = [B1, B2]

instance GenValid B
