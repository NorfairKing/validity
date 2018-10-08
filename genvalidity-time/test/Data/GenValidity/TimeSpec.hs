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
    shrinkValidSpec @Day
    genValidSpec @UniversalTime
    shrinkValidSpec @UniversalTime
    genValidSpec @DiffTime
    shrinkValidSpec @DiffTime
    genValiditySpec @UTCTime
    shrinkValiditySpec @UTCTime
    genValidSpec @NominalDiffTime
    shrinkValidSpec @NominalDiffTime
    genValidSpec @TimeZone
    shrinkValidSpec @TimeZone
    genValiditySpec @TimeOfDay
    shrinkValiditySpec @TimeOfDay
    genValiditySpec @LocalTime
    shrinkValiditySpec @LocalTime
    genValiditySpec @ZonedTime
    genValidSpec @TimeLocale
    shrinkValidSpec @TimeLocale
