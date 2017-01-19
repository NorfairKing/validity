{-# LANGUAGE TypeApplications #-}

module Test.Validity.OrdSpec where

import Test.Hspec

import Data.GenValidity
import Test.Validity.Ord

spec :: Spec
spec = do
    ordSpecOnValid @Double
    ordSpec @Int
    ordSpecOnArbitrary @Int
    ordSpecOnGen ((* 2) <$> genValid @Int) "even"
