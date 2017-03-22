{-# LANGUAGE TypeApplications #-}

module Test.Validity.BinarySpec where

import Test.Hspec

import Data.GenValidity
import Test.Validity.Binary

spec :: Spec
spec = do
    binarySpecOnGen (genListOf $ pure 'a') "sequence of 'a's"
    binarySpecOnValid @Double
    binarySpec @Int
    binarySpecOnArbitrary @Int
