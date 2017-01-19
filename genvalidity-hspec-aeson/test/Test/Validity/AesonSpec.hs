{-# LANGUAGE TypeApplications #-}

module Test.Validity.AesonSpec where

import Test.Hspec

import Data.GenValidity
import Test.Validity.Aeson

spec :: Spec
spec = do
    jsonSpecOnGen (genListOf $ pure 'a') "sequence of 'a's"
    jsonSpecOnValid @Double
    jsonSpec @Int
    jsonSpecOnArbitrary @Int
