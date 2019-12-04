{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Fixed
import Data.Int
import Data.List.NonEmpty (NonEmpty(..))
import Data.Ratio
import Data.Word
import Numeric.Natural

import Criterion.Main as Criterion

import Data.GenValidity.Criterion

main :: IO ()
main =
  Criterion.defaultMain
    [ genValidBench @()
    , genValidBench @Bool
    , genValidBench @Ordering
    , genValidBench @Char
    , genValidBench @Int
    , genValidBench @(Int, Int)
    , genValidBench @(Int, Int, Int)
    , genValidBench @(Int, Int, Int, Int)
    , genValidBench @(Maybe Int)
    , genValidBench @(Either Int Int)
    , genValidBench @[Int]
    , genValidBench @(NonEmpty Int)
    , genValidBench @(Ratio Int)
    , genValidBench @Int8
    , genValidBench @Int16
    , genValidBench @Int32
    , genValidBench @Int64
    , genValidBench @Integer
    , genValidBench @Word8
    , genValidBench @Word16
    , genValidBench @Word32
    , genValidBench @Word64
    , genValidBench @Natural
    , genValidBench @Float
    , genValidBench @Double
    , genValidBench @(Fixed E12)
    ]
