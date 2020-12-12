{-# LANGUAGE TypeApplications #-}

module Test.Syd.Validity.PersistSpec where

import Data.GenValidity
import Test.Syd
import Test.Syd.Validity.Persist

spec :: Spec
spec = do
  persistSpecOnGen (genListOf $ pure 'a') "sequence of 'a's" (const [])
  -- persistSpec @Double -- DOES NOT HOLD
  persistSpecOnValid @Rational
  persistSpec @Int
  persistSpecOnArbitrary @Int
