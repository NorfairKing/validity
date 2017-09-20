{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.Time.LocalTime where
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>))
import Data.Functor ((<$>))
#endif
import Data.GenValidity
import Data.GenValidity.Time.Calendar ()
import Data.Time.LocalTime
import Data.Validity.Time.LocalTime ()

import Test.QuickCheck

instance GenUnchecked TimeZone where
    genUnchecked = TimeZone <$> genUnchecked <*> genUnchecked <*> genUnchecked
    shrinkUnchecked (TimeZone m so n) =
        [TimeZone m' so' n' | (m', so', n') <- shrinkUnchecked (m, so, n)]

instance GenValid TimeZone where
    genValid = TimeZone <$> genValid <*> genValid <*> genValid

instance GenUnchecked TimeOfDay where
    genUnchecked = TimeOfDay <$> genUnchecked <*> genUnchecked <*> genUnchecked
    shrinkUnchecked (TimeOfDay h m s) =
        [TimeOfDay h' m' s' | (h', m', s') <- shrinkUnchecked (h, m, s)]

instance GenValid TimeOfDay where
    genValid =
        TimeOfDay <$> (genValid `suchThat` (>= 0) `suchThat` (<= 23)) <*>
        (genValid `suchThat` (>= 0) `suchThat` (<= 59)) <*>
        (genValid `suchThat` (>= 0) `suchThat` (<= 60))

instance GenInvalid TimeOfDay

instance GenUnchecked LocalTime where
    genUnchecked = LocalTime <$> genUnchecked <*> genUnchecked
    shrinkUnchecked (LocalTime d tod) =
        [LocalTime d' tod' | (d', tod') <- shrinkUnchecked (d, tod)]

instance GenValid LocalTime where
    genValid = LocalTime <$> genValid <*> genValid

instance GenInvalid LocalTime

instance GenUnchecked ZonedTime where
    genUnchecked = ZonedTime <$> genUnchecked <*> genUnchecked
    shrinkUnchecked (ZonedTime lt tz) =
        [ZonedTime lt' tz' | (lt', tz') <- shrinkUnchecked (lt, tz)]

instance GenValid ZonedTime where
    genValid = ZonedTime <$> genValid <*> genValid

instance GenInvalid ZonedTime
