{-# LANGUAGE TypeApplications #-}

module Test.Validity.ArbitrarySpec where

import Test.Hspec
import Test.Validity.Arbitrary

spec :: Spec
spec = arbitrarySpec @Int
