{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.GenValidity.Criterion
import Data.GenValidity.Time ()
import Data.Time

main :: IO ()
main =
  Criterion.defaultMain
    [ genValidBench @Day,
      genValidBench @UniversalTime,
      genValidBench @DiffTime,
      genValidBench @UTCTime,
      genValidBench @NominalDiffTime,
      -- , genValidBench @TimeLocale -- No NFData instance
      genValidBench @TimeZone,
      genValidBench @TimeOfDay,
      genValidBench @LocalTime,
      genValidBench @ZonedTime
    ]
