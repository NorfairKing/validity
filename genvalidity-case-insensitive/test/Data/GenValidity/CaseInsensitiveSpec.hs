{-# LANGUAGE TypeApplications #-}

module Data.GenValidity.CaseInsensitiveSpec
  ( spec,
  )
where

import Data.CaseInsensitive (CI)
import Data.GenValidity.CaseInsensitive ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  genValidSpec @(CI String)
  shrinkValidSpec @(CI String)
