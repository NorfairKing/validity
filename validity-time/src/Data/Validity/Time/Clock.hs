{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Validity.Time.Clock where

import Data.Validity

import Data.Time.Clock
import Data.Validity.Time.Calendar ()

-- | Valid according to the 'Rational' it contains.
instance Validity UniversalTime where
    isValid (ModJulianDate i) = isValid i
    validate = validateByCheckingName "UniversalTime"

-- | Trivially valid
instance Validity DiffTime where
    isValid = triviallyValid
    validate = validateByCheckingName "DiffTime"

instance Validity UTCTime where
    isValid UTCTime {..} =
        and
            [ isValid utctDay
            , isValid utctDayTime
            , utctDayTime >= 0
            , utctDayTime < 86401
            ]
    validate UTCTime {..} =
        mconcat
            [ utctDay <?!> "utctDay"
            , utctDayTime <?!> "utctDayTime"
            , utctDayTime >= 0 <?@> "The day time is positive."
            , utctDayTime < 86401 <?@> "The day time is strictly less than 86401."
            ]

instance Validity NominalDiffTime
    -- NominalDiffTime contains a 'Pico' but that constructorr is not exported so we can't do any better than this.
                                                                                                                    where
    isValid = isValid . (round :: NominalDiffTime -> Integer)
    validate = validateByCheckingName "NominalDiffTime"
