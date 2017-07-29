{-# LANGUAGE TypeApplications #-}

module Test.Validity.UnorderedContainersSpec where

import Test.Hspec

import Data.GenValidity.UnorderedContainers ()
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Test.Validity.GenValidity

spec :: Spec
spec = do
    genValidSpec @(HashSet Int)
    genValiditySpec @(HashSet Double)
    genValidSpec @(HashMap Int Double)
    genValiditySpec @(HashMap Double Double)
