{-# OPTIONS_GHC -Wno-orphans #-}
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

-- | Valid according to the validity of contained values
instance Validity LocalTime where
    isValid LocalTime {..} = isValid localDay && isValid localTimeOfDay

-- | Valid according to the validity of contained values
instance Validity ZonedTime where
    isValid ZonedTime {..} =
        isValid zonedTimeToLocalTime && isValid zonedTimeZone
