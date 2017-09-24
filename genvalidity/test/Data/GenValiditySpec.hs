module Data.GenValiditySpec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck

import Data.GenValidity

spec :: Spec
spec = do
    describe "upTo" $
        it "returns only positive integers" $
            forAll arbitrary $ \n -> forAll (upTo n) (`shouldSatisfy` (>= 0))
    describe "genSplit" $ do
        it "returns positive integers" $
            forAll arbitrary $ \i ->
                forAll (genSplit i) $ \(a, b) -> do
                    a `shouldSatisfy` (>= 0)
                    b `shouldSatisfy` (>= 0)
        it "returns two integers such that the sum is the original integer" $
            forAll (arbitrary `suchThat` (>= 0)) $ \i ->
                forAll (genSplit i) $ \(a, b) -> a + b `shouldBe` i
    describe "arbPartition" $ do
        it "returns an empty list upon strictly negative input" $
            forAll (arbitrary `suchThat` (< 0)) $ \n ->
                forAll (arbPartition n) (`shouldBe` [])
        it "returns a list of strictly positive integers" $
            forAll arbitrary $ \n ->
                forAll (arbPartition n) $ \p -> p `shouldSatisfy` all (> 0)
        it
            "returns a list of integers that sum to the original positive integer" $
            forAll (arbitrary `suchThat` (>= 0)) $ \n ->
                forAll (arbPartition n) $ \p -> sum p `shouldBe` n
