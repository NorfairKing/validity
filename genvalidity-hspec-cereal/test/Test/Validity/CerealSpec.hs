{-# LANGUAGE TypeApplications #-}

module Test.Validity.CerealSpec where

import Data.GenValidity
import Test.Hspec
import Test.Validity.Cereal

spec :: Spec
spec = do
  serializeSpecOnGen (genListOf $ pure 'a') "sequence of 'a's" (const [])
  -- serializeSpec @Double DOES NOT HOLD
  serializeSpec @Rational
  serializeSpec @Int
  serializeSpecOnArbitrary @Int
