{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Validity.Time.Clock where

import Data.Validity

import Data.Time.Clock
import Data.Validity.Time.Calendar ()

-- | Valid according to the 'Rational' it contains.
instance Validity UniversalTime where
    isValid (ModJulianDate i) = isValid i

-- | Trivially valid
instance Validity DiffTime where
    isValid = const True

instance Validity UTCTime where
    isValid UTCTime {..} =
        and
            [ isValid utctDay
            , isValid utctDayTime
            , utctDayTime >= 0
            , utctDayTime < 86401
            ]

instance Validity NominalDiffTime where
    isValid = isValid . (round :: NominalDiffTime -> Integer)
    -- NominalDiffTime contains a 'Pico' but that constructorr is not exported so we can't do any better than this.
