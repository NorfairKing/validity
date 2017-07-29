{-# LANGUAGE TypeApplications #-}

module Test.Validity.VectorSpec where

import Test.Hspec

import Data.GenValidity.Vector ()
import Data.Vector (Vector)
import Test.Validity.GenValidity

spec :: Spec
spec = do
    genValidSpec @(Vector Int)
    genValiditySpec @(Vector Double)
