{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Validity.Tree where

import Data.Validity

import Data.Tree

-- | A 'Tree' of things is valid if all the things in the 'Tree' are valid.
instance Validity a =>
         Validity (Tree a) where
    isValid = all isValid
