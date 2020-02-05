{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.Time.Calendar where

import Data.Validity

import Data.Time.Calendar

-- | Valid according to the 'Integer' it contains.
instance Validity Day where
  validate d@(ModifiedJulianDay i) =
    mconcat
      [ delve "toModifiedJulianDay" i
      , declare "The day happened after the beginning of time" $
        d > beginningOfTime
      ]

beginningOfTime :: Day
beginningOfTime = ModifiedJulianDay $ -((138 * (10 ^ (8 :: Integer))) * 356)
