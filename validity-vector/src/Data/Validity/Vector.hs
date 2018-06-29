{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.Vector where

import Data.Validity

import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV

import GHC.Exts as GHC (IsList(..))

-- | A 'Vector' of things is valid if all the elements are valid.
--
-- TODO make a more comprehensive instance that looks at implementation and
-- the underlying 'Array'
instance Validity a => Validity (V.Vector a) where
    validate hs = annotate (V.toList hs) "Vector elements"

instance (MUV.Unbox e, Validity e) => Validity (UV.Vector e) where
    validate = delve "Vector elements" . UV.toList

instance (MSV.Storable e, Validity e) => Validity (SV.Vector e) where
    validate = delve "Vector elements" . GHC.toList
