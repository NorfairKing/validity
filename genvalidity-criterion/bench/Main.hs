{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.Fixed
import Data.GenValidity.Criterion
import Data.Int
import Data.List.NonEmpty (NonEmpty (..))
import Data.Ratio
import Data.Word
import Numeric.Natural

main :: IO ()
main =
  Criterion.defaultMain
    [ genValidBench @(),
      genValidBench @Bool,
      genValidBench @Ordering,
      genValidBench @Char,
      genValidBench @Int,
      genValidBench @(Int, Int),
      genValidBench @(Int, Int, Int),
      genValidBench @(Int, Int, Int, Int),
      genValidBench @(Maybe Int),
      genValidBench @(Either Int Int),
      genValidBench @[Int],
      genValidBench @[[Int]],
      genValidBench @(NonEmpty Int),
      genValidBench @(NonEmpty (NonEmpty Int)),
      genValidBench @(Ratio Int),
      genValidBench @Int8,
      genValidBench @Int16,
      genValidBench @Int32,
      genValidBench @Int64,
      genValidBench @Integer,
      genValidBench @Word8,
      genValidBench @Word16,
      genValidBench @Word32,
      genValidBench @Word64,
      genValidBench @Natural,
      genValidBench @Float,
      genValidBench @Double,
      genValidBench @(Fixed E12),
      genValidBench @String
    ]
