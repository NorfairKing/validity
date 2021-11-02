{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Time.LocalTime where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>))
import Data.Functor ((<$>))
#endif
import Data.Fixed
import Data.GenValidity
import Data.GenValidity.Time.Calendar ()
import Data.GenValidity.Time.Clock ()
import Data.Time.Format
import Data.Time.LocalTime
import Data.Validity.Time.LocalTime ()
import Test.QuickCheck

instance GenUnchecked TimeZone where
  genUnchecked = TimeZone <$> genUnchecked <*> genUnchecked <*> genUnchecked
  shrinkUnchecked (TimeZone m so n) =
    [TimeZone m' so' n' | (m', so', n') <- shrinkUnchecked (m, so, n)]

instance GenValid TimeZone where
  genValid = TimeZone <$> genValid <*> genValid <*> genTimeZoneName
  shrinkValid = filter isValid . shrinkUnchecked

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

instance GenUnchecked TimeOfDay where
  genUnchecked = TimeOfDay <$> genUnchecked <*> genUnchecked <*> genUnchecked
  shrinkUnchecked (TimeOfDay h m s) =
    [TimeOfDay h' m' s' | (h', m', s') <- shrinkUnchecked (h, m, s)]

instance GenValid TimeOfDay where
  genValid =
    TimeOfDay <$> (choose (0, 23)) <*> (choose (0, 59))
      <*> (MkFixed <$> choose (0, 60999999999999))
  shrinkValid = filter isValid . shrinkUnchecked

instance GenInvalid TimeOfDay

instance GenUnchecked LocalTime where
  genUnchecked = LocalTime <$> genUnchecked <*> genUnchecked
  shrinkUnchecked (LocalTime d tod) =
    [LocalTime d' tod' | (d', tod') <- shrinkUnchecked (d, tod)]

instance GenValid LocalTime where
  genValid = LocalTime <$> genValid <*> genValid
  shrinkValid = filter isValid . shrinkUnchecked

instance GenInvalid LocalTime

instance GenUnchecked ZonedTime where
  genUnchecked = ZonedTime <$> genUnchecked <*> genUnchecked
  shrinkUnchecked (ZonedTime lt tz) =
    [ZonedTime lt' tz' | (lt', tz') <- shrinkUnchecked (lt, tz)]

instance GenValid ZonedTime where
  genValid = ZonedTime <$> genValid <*> genValid
  shrinkValid = filter isValid . shrinkUnchecked

instance GenInvalid ZonedTime

#if MIN_VERSION_time(1,9,0)
instance GenUnchecked CalendarDiffTime where
  genUnchecked = CalendarDiffTime <$> genUnchecked <*> genUnchecked
  shrinkUnchecked (CalendarDiffTime ms t) = [ CalendarDiffTime ms' t' | (ms', t') <- shrinkUnchecked (ms, t) ]

instance GenValid CalendarDiffTime where
  shrinkValid (CalendarDiffTime ms t) = [ CalendarDiffTime ms' t' | (ms', t') <- shrinkValid (ms, t) ]
  genValid = CalendarDiffTime <$> genValid <*> genValid
#endif
