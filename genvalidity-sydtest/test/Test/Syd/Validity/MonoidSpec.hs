{-# LANGUAGE TypeApplications #-}

module Test.Syd.Validity.MonoidSpec where

import Test.Syd
import Test.Syd.Validity.Monoid

spec :: Spec
spec = do
  monoidSpec @[Rational]
  monoidSpec @[Int]
  monoidSpecOnArbitrary @[Int]
  monoidSpecOnGen (pure "a") "singleton list of 'a'" (const [])
