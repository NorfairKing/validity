{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Time.Clock where

import Data.GenValidity
import Data.GenValidity.Time.Calendar ()
import Data.Time.Clock
import Data.Validity.Time.Clock ()
import Test.QuickCheck

instance GenValid UniversalTime where
  genValid = ModJulianDate <$> genValid
  shrinkValid = fmap ModJulianDate . shrinkValid . getModJulianDate

instance GenValid DiffTime where
  genValid = picosecondsToDiffTime <$> genValid
  shrinkValid = fmap picosecondsToDiffTime . shrinkValid . diffTimeToPicoseconds

instance GenValid UTCTime where
  genValid =
    UTCTime <$> genValid <*> (fromIntegral <$> choose (0 :: Int, 86400))
  shrinkValid (UTCTime d dt) =
    [UTCTime d' dt' | (d', dt') <- shrinkValid (d, dt)]

instance GenValid NominalDiffTime where
  genValid = secondsToNominalDiffTime <$> genValid
  shrinkValid = fmap secondsToNominalDiffTime . shrinkValid . nominalDiffTimeToSeconds
