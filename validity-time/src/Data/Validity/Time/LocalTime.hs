{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Validity.Time.LocalTime where

import Data.Validity

import Data.Time.LocalTime

import Data.Validity.Time.Calendar ()

-- | Valid according to the contained values.
instance Validity TimeZone where
    validate TimeZone {..} =
        mconcat
            [ timeZoneMinutes <?!> "timeZoneMinutes"
            , timeZoneSummerOnly <?!> "timeZoneSummerOnly"
            , timeZoneName <?!> "timeZoneName"
            ]

-- | Valid according to the validity of contained values and these constraints:
--
--  * todHour : range 0 - 23
--  * todMin : range 0 - 59
--  * todSec : 0 <= todSec < 61,
instance Validity TimeOfDay where
    validate TimeOfDay {..} =
        mconcat
            [ todHour <?!> "todHour"
            , todHour >= 0 <?@> "The 'hour' is positive."
            , todHour <= 23 <?@> "The 'hour' is 23 or less."
            , todMin <?!> "todMin"
            , todMin >= 0 <?@> "The 'minute' is positive."
            , todMin <= 59 <?@> "The 'minute' is 59 or less."
            , todSec <?!> "todSec"
            , todSec >= 0 <?@> "The 'second' is positive."
            , todSec < 61 <?@> "The 'second' is 60 or less."
            ]

-- | Valid according to the validity of contained values
instance Validity LocalTime where
    validate LocalTime {..} =
        mconcat [localDay <?!> "localDay", localTimeOfDay <?!> "localTimeOfDay"]

-- | Valid according to the validity of contained values
instance Validity ZonedTime where
    validate ZonedTime {..} =
        mconcat
            [ zonedTimeToLocalTime <?!> "zonedTimeToLocalTime"
            , zonedTimeZone <?!> "zonedTimeZone"
            ]
