{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Time.Format where

import Data.GenValidity
import Data.GenValidity.Time.LocalTime ()
import Data.Time.Format
import Data.Validity.Time.Format ()

instance GenValid TimeLocale where
  genValid =
    TimeLocale
      <$> genValid
      <*> genValid
      <*> genValid
      <*> genValid
      <*> genValid
      <*> genValid
      <*> genValid
      <*> genValid
  shrinkValid _ = []
