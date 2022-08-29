{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.GenValidity.Criterion
import Data.GenValidity.UUID ()
import Data.UUID

main :: IO ()
main =
  Criterion.defaultMain
    [ genValidBench @UUID,
      shrinkValidBench @UUID
    ]
