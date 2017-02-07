{-# OPTIONS_GHC -Wno-orphans #-}

module Data.GenValidity.Time.Clock where

import Data.GenValidity
import Data.GenValidity.Time.Calendar ()
import Data.Time.Clock
import Data.Validity.Time.Clock ()

instance GenUnchecked UniversalTime where
    genUnchecked = ModJulianDate <$> genUnchecked

instance GenValid UniversalTime

instance GenUnchecked DiffTime where
    genUnchecked = picosecondsToDiffTime <$> genUnchecked

instance GenValid DiffTime

instance GenUnchecked UTCTime where
    genUnchecked = UTCTime <$> genUnchecked <*> genUnchecked

instance GenValid UTCTime

instance GenInvalid UTCTime

instance GenUnchecked NominalDiffTime where
    genUnchecked = (fromIntegral :: Integer -> NominalDiffTime) <$> genUnchecked

instance GenValid NominalDiffTime
