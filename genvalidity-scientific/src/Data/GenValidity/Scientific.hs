{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.Scientific where

import Data.GenValidity
import Data.Scientific
import Data.Validity.Scientific ()

import Test.QuickCheck

instance GenUnchecked Scientific where
    genUnchecked = scientific <$> genUnchecked <*> genUnchecked

instance GenValid Scientific
