{-# LANGUAGE TypeApplications #-}

module Test.Validity.Containers.TreeSpec where

import Test.Hspec

import Data.GenValidity.Tree ()
import Data.Tree (Tree)
import Test.Validity.GenValidity

spec :: Spec
spec = do
    genValidSpec @(Tree Int)
    genValiditySpec @(Tree Double)
