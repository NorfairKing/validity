{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Time.LocalTime where

import Data.Fixed
import Data.GenValidity
import Data.GenValidity.Time.Calendar ()
import Data.GenValidity.Time.Clock ()
import Data.Time.Format
import Data.Time.LocalTime
import Data.Validity.Time.LocalTime ()
import Test.QuickCheck

instance GenValid TimeZone where
  genValid = TimeZone <$> genValid <*> genValid <*> genTimeZoneName
  shrinkValid (TimeZone m so n) =
    [TimeZone m' so' n' | (m', so', n') <- shrinkValid (m, so, n)]

genTimeZoneName :: Gen String
genTimeZoneName =
  frequency
    [ (1, pure ""),
      ( 4, -- Any three characters
        (:) <$> genValid
          <*> ((:) <$> genValid <*> ((:) <$> genValid <*> pure []))
      ),
      ( 4, -- A +HHMM string
        (:) <$> elements ['-', '+']
          <*> ( formatTime defaultTimeLocale "%H%M"
                  <$> (TimeOfDay <$> choose (0, 23) <*> choose (0, 59) <*> pure 0)
              )
      ),
      (1, genValid)
    ]

instance GenValid TimeOfDay where
  genValid =
    TimeOfDay <$> (choose (0, 23)) <*> (choose (0, 59))
      <*> (MkFixed <$> choose (0, 60999999999999))
  shrinkValid (TimeOfDay h m s) =
    filter isValid [TimeOfDay h' m' s' | (h', m', s') <- shrinkValid (h, m, s)]

instance GenValid LocalTime where
  genValid = LocalTime <$> genValid <*> genValid
  shrinkValid (LocalTime d tod) =
    filter isValid [LocalTime d' tod' | (d', tod') <- shrinkValid (d, tod)]

instance GenValid ZonedTime where
  genValid = ZonedTime <$> genValid <*> genValid
  shrinkValid (ZonedTime lt tz) =
    filter isValid [ZonedTime lt' tz' | (lt', tz') <- shrinkValid (lt, tz)]

instance GenValid CalendarDiffTime where
  genValid = CalendarDiffTime <$> genValid <*> genValid
  shrinkValid (CalendarDiffTime ms t) = [CalendarDiffTime ms' t' | (ms', t') <- shrinkValid (ms, t)]
