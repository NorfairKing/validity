{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.Time.Clock where

import Data.Time.Clock
import Data.Validity
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
      [ annotate utctDay "utctDay",
        annotate utctDayTime "utctDayTime",
        check (utctDayTime >= 0) "The day time is positive.",
        check (utctDayTime < 86401) "The day time is strictly less than 86401."
      ]

instance Validity NominalDiffTime where
  validate = trivialValidation
