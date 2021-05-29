{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.Aeson
import Data.GenValidity.Aeson ()
import Data.GenValidity.Criterion

main :: IO ()
main =
  Criterion.defaultMain
    [genValidBench @Array, genValidBench @Object, genValidBench @Value]
