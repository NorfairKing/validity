{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.GenValidity
import Data.GenValidity.Criterion
import Data.GenValidity.UUID ()
import Data.UUID
import Test.QuickCheck

main :: IO ()
main =
  Criterion.defaultMain
    [ genValidBench @UUID,
      genUncheckedBench @UUID,
      genBench "valid UUID via genUnchecked" ((genUnchecked `suchThat` isValid) :: Gen UUID)
    ]
