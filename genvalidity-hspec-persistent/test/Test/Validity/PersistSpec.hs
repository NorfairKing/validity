{-# LANGUAGE TypeApplications #-}

module Test.Validity.PersistSpec where

-- import Numeric.Natural
import Data.GenValidity
import Test.Hspec
import Test.Validity.Persist

spec :: Spec
spec = do
  persistSpecOnGen (genListOf $ pure 'a') "sequence of 'a's" (const [])
  -- persistSpec @Double -- DOES NOT HOLD
  persistSpec @Rational
  persistSpec @Int
  persistSpecOnArbitrary @Int
