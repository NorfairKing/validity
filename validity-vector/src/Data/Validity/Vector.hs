{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.Vector where

import Data.Validity

import Data.Vector (Vector)
import qualified Data.Vector as V

-- | A 'Vector' of things is valid if all the elements are valid.
--
-- TODO make a more comprehensive instance that looks at implementation and
-- the underlying 'Array'
instance (Validity a) => Validity (Vector a) where
    validate hs = annotate (V.toList hs) "Vector elements"
