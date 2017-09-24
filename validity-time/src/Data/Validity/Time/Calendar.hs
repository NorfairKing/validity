{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.Time.Calendar where

import Data.Validity

import Data.Time.Calendar

-- | Valid according to the 'Integer' it contains.
instance Validity Day where
    isValid (ModifiedJulianDay i) = isValid i
    validate = validateByCheckingName "Day"
