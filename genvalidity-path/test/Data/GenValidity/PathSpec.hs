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
  genValidSpec @(Path Abs File)
  shrinkValidSpec @(Path Abs File)
  genValidSpec @(Path Abs Dir)
  shrinkValidSpec @(Path Abs Dir)
  genValidSpec @(Path Rel File)
  shrinkValidSpec @(Path Rel File)
  genValidSpec @(Path Rel Dir)
  shrinkValidSpec @(Path Rel Dir)
