{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.Map where

import Data.Validity

import Data.Map (Map)
import qualified Data.Map as M

-- | A 'Map' of things is valid if all the keys and values are valid and the 'Map' itself
-- is valid.
instance (Ord k, Validity k, Validity v) => Validity (Map k v) where
    validate m =
        mconcat
            [ declare "The Map structure is valid." $ M.valid m
            , delve "Map elements" $ M.toList m
            ]
