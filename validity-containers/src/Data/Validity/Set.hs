{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.Set where

import Data.Containers.ListUtils
import Data.Set (Set)
import qualified Data.Set as S
import Data.Validity

-- | A 'Set' of things is valid if all the elements are valid and the 'Set' itself
-- is valid.
instance (Ord v, Validity v) => Validity (Set v) where
  validate s =
    mconcat
      [ declare "The set structure is valid." $ S.valid s,
        annotate (S.toList s) "Set elements"
      ]

distinctOrd :: (Ord a) => [a] -> Bool
distinctOrd ls = nubOrd ls == ls
