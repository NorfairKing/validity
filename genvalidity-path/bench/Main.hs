{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main
import Data.GenValidity.Criterion
import Data.GenValidity.Path ()
import Path

main :: IO ()
main =
  defaultMain
    [ bgroup
        "generators"
        [ genValidBench @(Path Abs File),
          genValidBench @(Path Rel File),
          genValidBench @(Path Abs Dir),
          genValidBench @(Path Rel Dir)
        ],
      bgroup
        "shrinkers"
        [ shrinkValidBench @(Path Abs File),
          shrinkValidBench @(Path Rel File),
          shrinkValidBench @(Path Abs Dir),
          shrinkValidBench @(Path Rel Dir)
        ]
    ]
