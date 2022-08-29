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
    [ bgroup
        "generators"
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
          genValidBench @Rational,
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
        ],
      bgroup
        "shrinkers"
        [ shrinkValidBench @(),
          shrinkValidBench @Bool,
          shrinkValidBench @Ordering,
          shrinkValidBench @Char,
          shrinkValidBench @Int,
          shrinkValidBench @(Int, Int),
          shrinkValidBench @(Int, Int, Int),
          shrinkValidBench @(Int, Int, Int, Int),
          shrinkValidBench @(Maybe Int),
          shrinkValidBench @(Either Int Int),
          shrinkValidBench @[Int],
          shrinkValidBench @[[Int]],
          shrinkValidBench @(NonEmpty Int),
          shrinkValidBench @(NonEmpty (NonEmpty Int)),
          shrinkValidBench @(Ratio Int),
          shrinkValidBench @Rational,
          shrinkValidBench @Int8,
          shrinkValidBench @Int16,
          shrinkValidBench @Int32,
          shrinkValidBench @Int64,
          shrinkValidBench @Integer,
          shrinkValidBench @Word8,
          shrinkValidBench @Word16,
          shrinkValidBench @Word32,
          shrinkValidBench @Word64,
          shrinkValidBench @Natural,
          shrinkValidBench @Float,
          shrinkValidBench @Double,
          shrinkValidBench @(Fixed E12),
          shrinkValidBench @String
        ]
    ]
