{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Validity.Time.Clock where

import Data.Validity

import Data.Time.Clock
import Data.Validity.Time.Calendar ()

-- | Valid according to the 'Rational' it contains.
instance Validity UniversalTime where
    validate = delve "toModifiedJulianDay" . getModJulianDate

-- | Trivially valid
instance Validity DiffTime where
    validate = trivialValidation

instance Validity UTCTime where
    validate UTCTime {..} =
        mconcat
            [ annotate utctDay $ "utctDay"
            , annotate utctDayTime $ "utctDayTime"
            , check (utctDayTime >= 0) "The day time is positive."
            , check
                  (utctDayTime < 86401)
                  "The day time is strictly less than 86401."
            ]

instance Validity NominalDiffTime
    -- NominalDiffTime contains a 'Pico' but that constructorr is not exported so we can't do any better than this.
                                                                                                                    where
    validate ndt =
        annotate
            ((round :: NominalDiffTime -> Integer) ndt)
            "round"
