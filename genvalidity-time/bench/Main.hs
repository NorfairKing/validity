{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Time

import Criterion.Main as Criterion

import Data.GenValidity.Criterion
import Data.GenValidity.Time ()

main :: IO ()
main =
  Criterion.defaultMain
    [ genValidBench @Day
    , genValidBench @UniversalTime
    , genValidBench @DiffTime
    , genValidBench @UTCTime
    , genValidBench @NominalDiffTime
    -- , genValidBench @TimeLocale -- No NFData instance
    , genValidBench @TimeZone
    , genValidBench @TimeOfDay
    , genValidBench @LocalTime
    , genValidBench @ZonedTime
    ]
