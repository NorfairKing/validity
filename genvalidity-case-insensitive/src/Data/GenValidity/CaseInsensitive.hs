{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.CaseInsensitive where

import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.GenValidity
import Data.Validity.CaseInsensitive ()

instance (Eq a, CI.FoldCase a, GenValid a) => GenValid (CI a) where
  genValid = CI.mk <$> genValid

  -- We have to filter out c again, because for values like String, case
  -- folding means we could end up with the same string again (according to
  -- Eq).
  shrinkValid c = filter (/= c) . fmap CI.mk . shrinkValid . CI.original $ c
