{-# LANGUAGE TypeApplications #-}

module Test.Validity.MonoidSpec where

import Test.Hspec
import Test.Validity.Monoid

spec :: Spec
spec = do
  monoidSpecOnValid @[Rational]
  monoidSpec @[Int]
  monoidSpecOnArbitrary @[Int]
  monoidSpecOnGen (pure "a") "singleton list of 'a'" (const [])
