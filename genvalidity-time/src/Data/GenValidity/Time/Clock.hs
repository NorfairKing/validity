{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.Time.Clock where
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>))
import Data.Functor ((<$>))
#endif
import Data.GenValidity
import Data.GenValidity.Time.Calendar ()
import Data.Time.Clock
import Data.Validity.Time.Clock ()

import Test.QuickCheck

instance GenUnchecked UniversalTime where
    genUnchecked = ModJulianDate <$> genUnchecked
    shrinkUnchecked = fmap ModJulianDate . shrinkUnchecked . getModJulianDate

instance GenValid UniversalTime where
    genValid = ModJulianDate <$> genValid

instance GenUnchecked DiffTime where
    genUnchecked = picosecondsToDiffTime <$> genUnchecked
#if MIN_VERSION_time(1,6,0)
    shrinkUnchecked =
        fmap picosecondsToDiffTime . shrinkUnchecked . diffTimeToPicoseconds
#else
    shrinkUnchecked = const []
#endif
instance GenValid DiffTime where
    genValid = picosecondsToDiffTime <$> genValid

instance GenUnchecked UTCTime where
    genUnchecked = UTCTime <$> genUnchecked <*> genUnchecked
    shrinkUnchecked (UTCTime d dt) =
        [UTCTime d' dt' | (d', dt') <- shrinkUnchecked (d, dt)]

instance GenValid UTCTime where
    genValid =
        UTCTime <$> genValid <*>
        (genValid `suchThat` (>= 0) `suchThat` (<= 86400))

instance GenInvalid UTCTime

instance GenUnchecked NominalDiffTime where
    genUnchecked = diffUTCTime <$> genUnchecked <*> genUnchecked
#if MIN_VERSION_time(1,9,0)
    shrinkUnchecked =
        fmap secondsToNominalDiffTime . shrinkUnchecked . nominalDiffTimeToSeconds
#else
    shrinkUnchecked = const []
#endif
instance GenValid NominalDiffTime
