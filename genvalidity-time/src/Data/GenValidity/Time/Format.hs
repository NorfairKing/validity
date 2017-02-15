{-# OPTIONS_GHC -Wno-orphans #-}

module Data.GenValidity.Time.Format where

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

instance GenValid TimeLocale
