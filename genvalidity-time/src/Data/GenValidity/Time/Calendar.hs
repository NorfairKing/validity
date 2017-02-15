{-# OPTIONS_GHC -Wno-orphans #-}

module Data.GenValidity.Time.Calendar where

import Data.GenValidity
import Data.Time.Calendar
import Data.Validity.Time.Calendar ()

instance GenUnchecked Day where
    genUnchecked = ModifiedJulianDay <$> genUnchecked

instance GenValid Day
