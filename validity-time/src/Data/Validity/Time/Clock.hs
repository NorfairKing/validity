{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Validity.Time.Clock where

import Data.Validity

import Data.Time.Clock

-- | Valid according to the 'Rational' it contains.
instance Validity UniversalTime where
    isValid (ModJulianDate i) = isValid i
