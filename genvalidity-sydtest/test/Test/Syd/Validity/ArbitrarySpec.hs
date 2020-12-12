{-# LANGUAGE TypeApplications #-}

module Test.Syd.Validity.ArbitrarySpec where

import Test.Syd

import Test.Syd.Validity.Arbitrary

spec :: Spec
spec = arbitrarySpec @Int
