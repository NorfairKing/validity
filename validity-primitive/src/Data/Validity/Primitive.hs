{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.Primitive where

import qualified Data.Primitive.Array as A
import Data.Validity

instance Validity a => Validity (A.Array a) where
  validate = foldMap validate
