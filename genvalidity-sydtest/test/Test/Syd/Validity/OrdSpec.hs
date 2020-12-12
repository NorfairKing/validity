{-# LANGUAGE TypeApplications #-}

module Test.Syd.Validity.OrdSpec where

import Data.GenValidity
import Test.Syd
import Test.Syd.Validity.Ord

spec :: Spec
spec = do
  ordSpecOnValid @Rational
  ordSpec @Int
  ordSpecOnArbitrary @Int
  ordSpecOnGen ((* 2) <$> genValid @Int) "even" (const [])
