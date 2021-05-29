{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.Tree where

import Data.Tree
import Data.Validity

-- | A 'Tree' of things is valid if all the things in the 'Tree' are valid.
instance Validity a => Validity (Tree a) where
  validate (Node rl sf) =
    mconcat [annotate rl "rootLabel", annotate sf "subForest"]
