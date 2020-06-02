{-# LANGUAGE TypeApplications #-}

module Data.GenValidity.Containers.TreeSpec where

import Data.GenValidity.Tree ()
import Data.Tree (Tree)
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  genValidSpec @(Tree Int)
  genValiditySpec @(Tree Rational)
