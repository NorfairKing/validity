{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.TimeSpec
  ( spec,
  )
where

import Data.GenValidity.Time ()
import Data.Time
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  genValidSpec @Day
  genValidSpec @UniversalTime
  genValidSpec @DiffTime
  genValiditySpec @UTCTime
  genValidSpec @NominalDiffTime
  genValidSpec @TimeZone
  genValiditySpec @TimeOfDay
  genValiditySpec @LocalTime
  genValiditySpec @ZonedTime
  genValidSpec @TimeLocale
#if MIN_VERSION_time(1,9,0)
  genValidSpec @CalendarDiffDays
  genValidSpec @DayOfWeek
  genValidSpec @CalendarDiffTime
#endif
