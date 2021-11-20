{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.Time.Format where

import Data.Time.Format
import Data.Validity
import Data.Validity.Time.LocalTime ()

-- | Valid according to the contained values
instance Validity TimeLocale where
  validate TimeLocale {..} =
    mconcat
      [ annotate wDays "wDays",
        annotate months "months",
        annotate amPm "amPm",
        annotate dateTimeFmt "dateTimeFmt",
        annotate dateFmt "dateFmt",
        annotate timeFmt "timeFmt",
        annotate time12Fmt "time12Fmt",
        annotate knownTimeZones "knownTimeZones"
      ]
