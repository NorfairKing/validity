{-# LANGUAGE TypeApplications #-}

module Test.Validity.UnorderedContainersSpec where

import Data.GenValidity.UnorderedContainers ()
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Test.Hspec
import Test.Validity.GenValidity

spec :: Spec
spec = do
  genValidSpec @(HashSet Int)
  genValiditySpec @(HashSet Rational)
  genValidSpec @(HashMap Int Rational)
  genValiditySpec @(HashMap Rational Rational)
