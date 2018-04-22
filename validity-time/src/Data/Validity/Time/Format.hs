{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#if MIN_VERSION_time(1,5,0)
{-# LANGUAGE RecordWildCards #-}
#endif
module Data.Validity.Time.Format where

import Data.Validity

import Data.Time.Format

import Data.Validity.Time.LocalTime ()
#if MIN_VERSION_time(1,5,0)
-- | Valid according to the contained values
instance Validity TimeLocale where
    validate TimeLocale {..} =
        mconcat
            [ annotate wDays $ "wDays"
            , annotate months $ "months"
            , annotate amPm $ "amPm"
            , annotate dateTimeFmt $ "dateTimeFmt"
            , annotate dateFmt $ "dateFmt"
            , annotate timeFmt $ "timeFmt"
            , annotate time12Fmt $ "time12Fmt"
            , annotate knownTimeZones $ "knownTimeZones"
            ]
#endif
