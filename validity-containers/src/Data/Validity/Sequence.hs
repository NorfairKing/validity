{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.Sequence where

import Data.Validity

import Data.Foldable (toList)
import Data.Sequence (Seq)

-- | A 'Seq'uence of things is valid if all the elements are valid.
instance Validity v => Validity (Seq v) where
    validate s = annotate (toList s) "Seq elements"
