module Data.Validity.Sequence where

import Data.Validity

import Data.Sequence (Seq)
import qualified Data.Sequence as S

-- | A 'Seq'uence of things is valid if all the elements are valid.
instance (Ord v, Validity v) =>
         Validity (Seq v) where
    isValid = all isValid
