{-# LANGUAGE TypeApplications #-}

module Test.Validity.BinarySpec where

import Data.GenValidity
import Test.Hspec
import Test.Validity.Binary

spec :: Spec
spec = do
  binarySpecOnGen (genListOf $ pure 'a') "sequence of 'a's" (const [])
  -- binarySpec @Double DOES NOT HOLD
  binarySpec @Rational
  binarySpec @Int
  binarySpecOnArbitrary @Int
