{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.DList () where

import Data.DList (DList)
import qualified Data.DList as DList
import Data.GenValidity (GenValid (..))
import Data.Validity.DList ()

instance (GenValid a) => GenValid (DList a) where
  genValid = DList.fromList <$> genValid
  shrinkValid = fmap DList.fromList . shrinkValid . DList.toList
