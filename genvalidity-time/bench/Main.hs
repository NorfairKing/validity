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
    [ bgroup
        "generators"
        [ genValidBench @Day,
          genValidBench @CalendarDiffDays,
          genValidBench @DayOfWeek,
          genValidBench @UniversalTime,
          genValidBench @DiffTime,
          genValidBench @UTCTime,
          genValidBench @NominalDiffTime,
          -- , genValidBench @TimeLocale -- No NFData instance
          genValidBench @TimeZone,
          genValidBench @TimeOfDay,
          genValidBench @LocalTime,
          genValidBench @ZonedTime
        ],
      bgroup
        "shrinkers"
        [ shrinkValidBench @Day,
          shrinkValidBench @CalendarDiffDays,
          shrinkValidBench @DayOfWeek,
          shrinkValidBench @UniversalTime,
          shrinkValidBench @DiffTime,
          shrinkValidBench @UTCTime,
          shrinkValidBench @NominalDiffTime,
          -- , shrinkValidBench @TimeLocale -- No NFData instance
          shrinkValidBench @TimeZone,
          shrinkValidBench @TimeOfDay,
          shrinkValidBench @LocalTime,
          shrinkValidBench @ZonedTime
        ]
    ]
