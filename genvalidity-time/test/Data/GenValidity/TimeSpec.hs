{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.TimeSpec
  ( spec,
  )
where

import Data.GenValidity.Time
import Data.Time
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  genValidSpec @Day
  describe "genSmartDayAround" $ it "generates valid days" $ forAllValid $ \ d -> genGeneratesValid $ genSmartDayAround d
  describe "genDayAround" $ it "generates valid days" $ forAllValid $ \ d -> genGeneratesValid $ genDayAround d
  describe "genDayCloselyAround" $ it "generates valid days" $ forAllValid $ \d -> genGeneratesValid $ genDayCloselyAround d
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
