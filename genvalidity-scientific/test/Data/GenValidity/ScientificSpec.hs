{-# LANGUAGE TypeApplications #-}

module Data.GenValidity.ScientificSpec
    ( spec
    ) where

import Test.Hspec
import Test.Validity

import Data.GenValidity.Scientific ()

import Data.Scientific

spec :: Spec
spec = do
    genValidSpec @Scientific
    shrinkValidSpec @Scientific
