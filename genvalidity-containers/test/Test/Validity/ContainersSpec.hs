{-# LANGUAGE TypeApplications #-}

module Test.Validity.ContainersSpec where

import Test.Hspec

import Data.GenValidity.Containers ()
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Tree (Tree)
import Test.Validity.GenValidity

spec :: Spec
spec = do
    genValidSpec @(Set Int)
    genValiditySpec @(Set Double)
    genValidSpec @(Map Int Double)
    genValiditySpec @(Map Double Double)
    genValidSpec @(Tree Int)
    genValiditySpec @(Tree Double)
    genValidSpec @(Seq Int)
    genValiditySpec @(Seq Double)
