{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.Time.Calendar where

import Data.Time.Calendar
import Data.Validity

-- | Valid according to the 'Integer' it contains.
instance Validity Day where
  validate (ModifiedJulianDay i) =
    mconcat
      [ delve "toModifiedJulianDay" i
      ]
