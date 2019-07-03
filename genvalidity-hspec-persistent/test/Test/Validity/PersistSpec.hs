{-# LANGUAGE TypeApplications #-}

module Test.Validity.PersistSpec where

import Test.Hspec

import Data.GenValidity
import Test.Validity.Persist

spec :: Spec
spec = do
  persistSpecOnGen (genListOf $ pure 'a') "sequence of 'a's" (const [])
  -- persistSpec @Double -- DOES NOT HOLD
  persistSpecOnValid @Rational
  persistSpec @Int
  persistSpecOnArbitrary @Int
