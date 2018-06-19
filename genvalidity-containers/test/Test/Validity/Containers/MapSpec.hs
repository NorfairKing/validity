{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module Test.Validity.Containers.MapSpec where

import Test.Hspec

import Data.GenValidity.Map ()
import Data.Map (Map)
import Test.Validity.GenValidity

spec :: Spec
spec = do
    genValidSpec @(Map Int Double)
    genValiditySpec @(Map Double Double)
