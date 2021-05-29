{-# LANGUAGE TypeApplications #-}

module Data.GenValidity.PathSpec
  ( spec,
  )
where

import Data.GenValidity.Path ()
import Path
import Test.Hspec
import Test.Validity

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
