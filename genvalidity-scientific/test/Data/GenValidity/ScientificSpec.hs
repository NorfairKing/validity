{-# LANGUAGE TypeApplications #-}

module Data.GenValidity.ScientificSpec
  ( spec,
  )
where

import Data.GenValidity.Scientific ()
import Data.Scientific
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  genValidSpec @Scientific

-- shrinkValidSpec @Scientific
