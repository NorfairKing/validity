{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.Time.Calendar where

import Data.Time.Calendar
import Data.Validity

-- | Valid according to the 'Integer' it contains.
instance Validity Day where
  validate (ModifiedJulianDay i) =
    mconcat
      [ delve "toModifiedJulianDay" i
      ]

#if MIN_VERSION_time(1,9,0)
instance Validity CalendarDiffDays where
  validate (CalendarDiffDays ms ds) =
    mconcat
      [ delve "cdMonths" ms,
        delve "cdDays" ds
      ]

instance Validity DayOfWeek where
  validate = trivialValidation
#endif
