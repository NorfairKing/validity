{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.Tree where

import Data.Validity

import Data.Tree

-- | A 'Tree' of things is valid if all the things in the 'Tree' are valid.
instance Validity a => Validity (Tree a) where
    isValid (Node rl sf) = isValid rl && isValid sf
    validate (Node rl sf) = mconcat [rl <?!> "rootLabel", sf <?!> "subForest"]
