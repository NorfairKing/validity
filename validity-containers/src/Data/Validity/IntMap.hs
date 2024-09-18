{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.IntMap
  ( decorateIntMap,
  )
where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Validity

-- | A 'IntMap' of things is valid if all the keys and values are valid and the 'IntMap' itself
-- is valid.
instance (Validity v) => Validity (IntMap v) where
  validate m =
    decorate "IntMap elements" $
      decorateIntMap m validate

decorateIntMap :: IntMap v -> (v -> Validation) -> Validation
decorateIntMap m func = IM.foldMapWithKey go m
  where
    go k v = decorate ("The key/value at key " ++ show k) $ func v
