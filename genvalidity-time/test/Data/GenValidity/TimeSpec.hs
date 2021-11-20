{-# LANGUAGE TypeApplications #-}

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
  describe "genSmartDayAround" $ it "generates valid days" $ forAllValid $ \d -> genGeneratesValid $ genSmartDayAround d
  describe "genDayAround" $ it "generates valid days" $ forAllValid $ \d -> genGeneratesValid $ genDayAround d
  describe "genDayCloselyAround" $ it "generates valid days" $ forAllValid $ \d -> genGeneratesValid $ genDayCloselyAround d
  genValidSpec @UniversalTime
  genValidSpec @DiffTime
  genValidSpec @UTCTime
  genValidSpec @NominalDiffTime
  genValidSpec @TimeZone
  genValidSpec @TimeOfDay
  genValidSpec @LocalTime
  genValidSpec @ZonedTime
  genValidSpec @TimeLocale
  genValidSpec @CalendarDiffDays
  genValidSpec @DayOfWeek
  genValidSpec @CalendarDiffTime
