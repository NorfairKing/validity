{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Validity.Time.Clock where

import Data.Validity

import Data.Time.Clock
import Data.Validity.Time.Calendar ()

-- | Valid according to the 'Rational' it contains.
instance Validity UniversalTime where
    isValid (ModJulianDate i) = isValid i

-- | Valid according to the validity of the result of 'diffTimeToPicoseconds'
instance Validity DiffTime where
    isValid = isValid . diffTimeToPicoseconds

instance Validity UTCTime where
    isValid UTCTime {..} =
        and
            [ isValid utctDay
            , isValid utctDayTime
            , diffTimeToPicoseconds utctDayTime >= 0
            , diffTimeToPicoseconds utctDayTime < 86401 * 10 ^ 12
            ]

instance Validity NominalDiffTime where
    isValid = isValid . (round :: NominalDiffTime -> Integer)
    -- NominalDiffTime contains a 'Pico' but that constructorr is not exported so we can't do any better than this.
