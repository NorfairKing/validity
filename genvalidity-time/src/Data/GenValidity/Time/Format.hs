{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.Time.Format where
#if MIN_VERSION_time(1,5,0)
import Data.GenValidity
import Data.GenValidity.Time.LocalTime ()
import Data.Time.Format
import Data.Validity.Time.Format ()

instance GenUnchecked TimeLocale where
    genUnchecked =
        TimeLocale <$> genUnchecked <*> genUnchecked <*> genUnchecked <*>
        genUnchecked <*>
        genUnchecked <*>
        genUnchecked <*>
        genUnchecked <*>
        genUnchecked
    shrinkUnchecked _ = []

instance GenValid TimeLocale where
    genValid =
        TimeLocale <$> genValid <*> genValid <*> genValid <*> genValid <*>
        genValid <*>
        genValid <*>
        genValid <*>
        genValid
#endif
