{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Validity.Time.LocalTime where

import Data.Validity

import Data.Time.LocalTime

import Data.Validity.Time.Calendar ()

-- | Valid according to the contained values.
instance Validity TimeZone where
    isValid TimeZone {..} =
        isValid timeZoneMinutes &&
        isValid timeZoneSummerOnly && isValid timeZoneName
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
    isValid TimeOfDay {..} =
        and
            [ isValid todHour
            , todHour >= 0
            , todHour <= 23
            , isValid todMin
            , todMin >= 0
            , todMin <= 59
            , isValid todSec
            , todSec >= 0
            , todSec < 61
            ]
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
    isValid LocalTime {..} = isValid localDay && isValid localTimeOfDay
    validate LocalTime {..} =
        mconcat [localDay <?!> "localDay", localTimeOfDay <?!> "localTimeOfDay"]

-- | Valid according to the validity of contained values
instance Validity ZonedTime where
    isValid ZonedTime {..} =
        isValid zonedTimeToLocalTime && isValid zonedTimeZone
    validate ZonedTime {..} =
        mconcat
            [ zonedTimeToLocalTime <?!> "zonedTimeToLocalTime"
            , zonedTimeZone <?!> "zonedTimeZone"
            ]
