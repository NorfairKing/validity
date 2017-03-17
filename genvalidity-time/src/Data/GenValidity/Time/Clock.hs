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

instance GenUnchecked UniversalTime where
    genUnchecked = ModJulianDate <$> genUnchecked

instance GenValid UniversalTime

instance GenUnchecked DiffTime where
    genUnchecked = picosecondsToDiffTime <$> genUnchecked

instance GenValid DiffTime

instance GenUnchecked UTCTime where
    genUnchecked = UTCTime <$> genUnchecked <*> genUnchecked

instance GenValid UTCTime

instance GenInvalid UTCTime

instance GenUnchecked NominalDiffTime where
    genUnchecked = (fromIntegral :: Integer -> NominalDiffTime) <$> genUnchecked

instance GenValid NominalDiffTime
