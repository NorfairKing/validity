{-# LANGUAGE TypeApplications #-}

module Test.Validity.AesonSpec where

import Test.Hspec

import Data.GenValidity.Aeson ()
import Test.Validity.GenValidity
import Data.Aeson (Value)

spec :: Spec
spec = genValiditySpec @Value
