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

instance GenUnchecked TimeZone where
    genUnchecked = TimeZone <$> genUnchecked <*> genUnchecked <*> genUnchecked

instance GenValid TimeZone

instance GenUnchecked TimeOfDay where
    genUnchecked = TimeOfDay <$> genUnchecked <*> genUnchecked <*> genUnchecked

instance GenValid TimeOfDay

instance GenInvalid TimeOfDay

instance GenUnchecked LocalTime where
    genUnchecked = LocalTime <$> genUnchecked <*> genUnchecked

instance GenValid LocalTime

instance GenInvalid LocalTime

instance GenUnchecked ZonedTime where
    genUnchecked = ZonedTime <$> genUnchecked <*> genUnchecked

instance GenValid ZonedTime

instance GenInvalid ZonedTime
