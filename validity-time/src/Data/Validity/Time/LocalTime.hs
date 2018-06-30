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
            [ annotate timeZoneMinutes "timeZoneMinutes"
            , annotate timeZoneSummerOnly "timeZoneSummerOnly"
            , annotate timeZoneName "timeZoneName"
            ]

-- | Valid according to the validity of contained values and these constraints:
--
--  * todHour : range 0 - 23
--  * todMin : range 0 - 59
--  * todSec : 0 <= todSec < 61,
instance Validity TimeOfDay where
    validate TimeOfDay {..} =
        mconcat
            [ annotate todHour "todHour"
            , check (todHour >= 0) "The 'hour' is positive."
            , check (todHour <= 23) "The 'hour' is 23 or less."
            , annotate todMin "todMin"
            , check (todMin >= 0) "The 'minute' is positive."
            , check (todMin <= 59) "The 'minute' is 59 or less."
            , annotate todSec "todSec"
            , check (todSec >= 0) "The 'second' is positive."
            , check (todSec < 61) "The 'second' is 60 or less."
            ]

-- | Valid according to the validity of contained values
instance Validity LocalTime where
    validate LocalTime {..} =
        mconcat
            [ annotate localDay "localDay"
            , annotate localTimeOfDay "localTimeOfDay"
            ]

-- | Valid according to the validity of contained values
instance Validity ZonedTime where
    validate ZonedTime {..} =
        mconcat
            [ annotate zonedTimeToLocalTime "zonedTimeToLocalTime"
            , annotate zonedTimeZone "zonedTimeZone"
            ]
