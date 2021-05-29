{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.Map
  ( decorateMap,
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Validity

-- | A 'Map' of things is valid if all the keys and values are valid and the 'Map' itself
-- is valid.
instance (Show k, Ord k, Validity k, Validity v) => Validity (Map k v) where
  validate m =
    mconcat
      [ declare "The Map structure is valid." $ M.valid m,
        decorate "Map elements" $
          decorateMap m $ \k v -> mconcat [delve "The key" k, delve "The value" v]
      ]

decorateMap :: Show k => Map k v -> (k -> v -> Validation) -> Validation
decorateMap m func = M.foldMapWithKey go m
  where
    go k v = decorate ("The key/value at key " ++ show k) $ func k v
