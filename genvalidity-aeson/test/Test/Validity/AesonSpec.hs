{-# LANGUAGE TypeApplications #-}

module Test.Validity.AesonSpec where

import Test.Hspec

import Data.Aeson (Value)
import Data.GenValidity.Aeson ()
import Test.Validity.GenValidity

spec :: Spec
spec = genValidSpec @Value
