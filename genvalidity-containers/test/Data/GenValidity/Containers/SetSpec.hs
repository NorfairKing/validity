{-# LANGUAGE TypeApplications #-}

module Data.GenValidity.Containers.SetSpec where

import Data.GenValidity
import Data.GenValidity.Set
import Data.Set (Set)
import Data.Validity.Containers
import Test.Hspec
import Test.QuickCheck
import Test.Validity

spec :: Spec
spec = do
  describe "genSetOf" $
    it "produces valid sets" $
      genGeneratesValid
        (genSetOf @Rational genValid)
  genValidSpec @(Set Int)
  genValidSpec @(Set Rational)
  describe "genSeperate" $ do
    it "generates values that are seperate" $
      forAll (genSeperate genValid) $
        \ls -> distinctOrd (ls :: [Int])
    it "generates values that are seperate" $
      forAll (genSeperate genValid) $
        \ls -> distinctOrd (ls :: [Int])
  describe "genSeperateFor" $ do
    it "generates values that are seperate" $
      forAllValid $ \ls ->
        forAll (genSeperateFor genValid ls) $ \tups -> distinctOrd (map fst (tups :: [(Int, Int)]))
    it "generates values that are seperate" $
      forAllValid $ \ls ->
        forAll (genSeperateFor genValid ls) $ \tups -> distinctOrd (map fst (tups :: [(Int, Int)]))
  describe "genValidSeperateFor" $
    it "generates values that are seperate" $
      forAllValid $ \ls ->
        forAll (genValidSeperateFor ls) $ \tups -> distinctOrd (map fst (tups :: [(Int, Int)]))
