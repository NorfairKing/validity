{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.CaseInsensitive where

import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Validity

instance (Validity a, Eq a, CI.FoldCase a) => Validity (CI a) where
  validate ci =
    mconcat
      [ delve "original" $ CI.original ci,
        delve "foldedCase" $ CI.foldedCase ci,
        -- The following could go wrong when using unsafeMk:
        declare "The foldedCase is indeed folded correctly" $
          CI.foldedCase ci == CI.foldCase (CI.original ci)
      ]
