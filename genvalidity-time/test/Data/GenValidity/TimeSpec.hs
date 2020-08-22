{-# LANGUAGE TypeApplications #-}

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
  genValidSpec @CalendarDiffDays
  genValidSpec @UniversalTime
  genValidSpec @DiffTime
  genValiditySpec @UTCTime
  genValidSpec @NominalDiffTime
  genValidSpec @TimeZone
  genValiditySpec @TimeOfDay
  genValiditySpec @LocalTime
  genValiditySpec @ZonedTime
  genValidSpec @TimeLocale
