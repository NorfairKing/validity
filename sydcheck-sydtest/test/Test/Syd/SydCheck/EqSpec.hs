{-# LANGUAGE TypeApplications #-}

module Test.Syd.SydCheck.EqSpec where

import Test.Syd
import Test.Syd.SydCheck
import Test.Syd.SydCheck.Eq
import Test.SydCheck

spec :: Spec
spec = do
  eqSpec @Int
  eqSpecOnGen ((* 2) <$> genValid @Int) "even"
