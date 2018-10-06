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
    shrinkValiditySpec @(Path Abs File)
    genValiditySpec @(Path Abs Dir)
    shrinkValiditySpec @(Path Abs Dir)
    genValiditySpec @(Path Rel File)
    shrinkValiditySpec @(Path Rel File)
    genValiditySpec @(Path Rel Dir)
    shrinkValiditySpec @(Path Rel Dir)
