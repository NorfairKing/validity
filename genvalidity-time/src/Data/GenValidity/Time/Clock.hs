{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
  shrinkValid = fmap ModJulianDate . shrinkValid . getModJulianDate

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

#if MIN_VERSION_time(1,6,0)
  shrinkValid =
    fmap picosecondsToDiffTime . shrinkValid . diffTimeToPicoseconds
#else
  shrinkValid = const []
#endif

instance GenUnchecked UTCTime where
  genUnchecked = UTCTime <$> genUnchecked <*> genUnchecked
  shrinkUnchecked (UTCTime d dt) =
    [UTCTime d' dt' | (d', dt') <- shrinkUnchecked (d, dt)]

instance GenValid UTCTime where
  genValid =
    UTCTime <$> genValid <*> (fromIntegral <$> choose (0 :: Int, 86400))
  shrinkValid (UTCTime d dt) =
    [UTCTime d' dt' | (d', dt') <- shrinkValid (d, dt)]

instance GenInvalid UTCTime

#if MIN_VERSION_time(1,9,1)
instance GenUnchecked NominalDiffTime where
  genUnchecked = secondsToNominalDiffTime <$> genUnchecked
#else
instance GenUnchecked NominalDiffTime where
  genUnchecked = diffUTCTime <$> genUnchecked <*> genUnchecked
#endif
#if MIN_VERSION_time(1,9,0)
  shrinkUnchecked =
    fmap secondsToNominalDiffTime . shrinkUnchecked . nominalDiffTimeToSeconds
#else
  shrinkUnchecked = const []
#endif
instance GenValid NominalDiffTime where
  genValid = genUnchecked
  shrinkValid = shrinkUnchecked
