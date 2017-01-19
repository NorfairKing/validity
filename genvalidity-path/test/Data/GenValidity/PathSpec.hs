{-# LANGUAGE TypeApplications #-}

module Data.GenValidity.PathSpec
    ( spec
    ) where

import Path

import Test.Hspec
import Test.Validity

import Data.GenValidity.Path ()

spec :: Spec
spec = do
    genValiditySpec @(Path Abs File)
    genValiditySpec @(Path Abs Dir)
    genValiditySpec @(Path Rel File)
    genValiditySpec @(Path Rel Dir)
