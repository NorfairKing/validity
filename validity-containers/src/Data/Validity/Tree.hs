module Data.Validity.Tree where

import           Data.Validity

import           Data.Tree

-- | A tree of things is valid if all the things are valid
instance Validity a => Validity (Tree a) where
    isValid = all isValid


