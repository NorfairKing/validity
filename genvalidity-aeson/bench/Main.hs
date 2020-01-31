{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Aeson

import Criterion.Main as Criterion

import Data.GenValidity.Aeson ()
import Data.GenValidity.Criterion

main :: IO ()
main =
  Criterion.defaultMain
    [genValidBench @Array, genValidBench @Object, genValidBench @Value]
