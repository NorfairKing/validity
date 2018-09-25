{-# LANGUAGE TypeApplications #-}

module Test.Validity.ReadShowSpec where

import Test.Hspec

import Data.GenValidity
import Test.Validity.ReadShow

spec :: Spec
spec = do
    readShowSpec @Int
    readShowSpecOnArbitrary @Int
    readShowSpecOnValid @Int
    readShowSpecOnGen ((* 2) <$> genValid @Int) "even" (const [])
