{-# LANGUAGE TypeApplications #-}

module Data.GenValidity.TimeSpec
  ( spec
  ) where

import Test.Hspec
import Test.Validity

import Data.GenValidity.Time ()
import Data.Time

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
