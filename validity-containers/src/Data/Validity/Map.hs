module Data.Validity.Map where

import           Data.Validity

import           Data.Map (Map)
import qualified Data.Map as M

-- | A tree of things is valid if all the things are valid and the map itself
-- is valid.
instance (Ord k, Validity k, Validity v) => Validity (Map k v) where
    isValid m = M.valid m && all isValid (M.keys m) && all isValid m


