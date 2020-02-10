{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.Time.Calendar where

import Data.GenValidity
import Data.Time.Calendar
import Data.Validity.Time.Calendar ()
#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$>))
#endif
import Test.QuickCheck

instance GenUnchecked Day where
  genUnchecked = ModifiedJulianDay <$> genUnchecked
  shrinkUnchecked _ = []

instance GenValid Day where
  genValid = (ModifiedJulianDay <$> genValid) `suchThat` isValid
