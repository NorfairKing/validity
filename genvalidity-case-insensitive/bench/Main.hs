{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main
import Data.CaseInsensitive (CI)
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Criterion

main :: IO ()
main =
  defaultMain
    [ genValidBench @(CI String)
    ]
