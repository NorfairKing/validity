{-# LANGUAGE TypeApplications #-}

module Test.Validity.Containers.SetSpec where

import Test.Hspec

import Data.GenValidity.Set ()
import Data.Set (Set)
import Test.Validity.GenValidity

spec :: Spec
spec = do
    genValidSpec @(Set Int)
    genValiditySpec @(Set Double)
