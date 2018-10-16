{-# LANGUAGE TypeApplications #-}

module Test.Validity.CerealSpec where

import Test.Hspec

import Data.GenValidity
import Test.Validity.Cereal

spec :: Spec
spec = do
    serializeSpecOnGen (genListOf $ pure 'a') "sequence of 'a's" (const [])
    serializeSpecOnValid @Rational
    serializeSpec @Int
    serializeSpecOnArbitrary @Int
