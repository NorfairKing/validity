{-# LANGUAGE TypeApplications #-}

module Test.Validity.EqSpec where

import Test.Hspec

import Data.GenValidity
import Test.Validity.Eq

spec :: Spec
spec = do
    eqSpecOnValid @Double
    eqSpec @Int
    eqSpecOnArbitrary @Int
    eqSpecOnGen ((* 2) <$> genValid @Int) "even"
