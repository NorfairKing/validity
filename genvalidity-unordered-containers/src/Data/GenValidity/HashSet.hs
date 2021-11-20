{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.HashSet where

import Data.GenValidity
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.Validity.HashSet ()

instance (Hashable v, Eq v, GenValid v) => GenValid (HashSet v) where
  genValid = HS.fromList <$> genValid
  shrinkValid = fmap HS.fromList . shrinkValid . HS.toList
