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
    [ bgroup
        "generators"
        [ genValidBench @Array,
          genValidBench @Object,
          genValidBench @Value
        ],
      bgroup
        "shrinkers"
        [ shrinkValidBench @Array,
          shrinkValidBench @Object,
          shrinkValidBench @Value
        ]
    ]
