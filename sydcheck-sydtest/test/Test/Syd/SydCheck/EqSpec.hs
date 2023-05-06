{-# LANGUAGE TypeApplications #-}

module Test.Syd.SydCheck.EqSpec where

import SydCheck
import Test.Syd
import Test.Syd.SydCheck
import Test.Syd.SydCheck.Eq

spec :: Spec
spec = do
  eqSpec @Int
  eqSpecOnGen ((* 2) <$> genValid @Int) "even"
