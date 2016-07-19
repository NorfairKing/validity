module Data.Validity.Map where

import           Data.Validity

import           Data.Map (Map)
import qualified Data.Map as M

-- | A tree of things is valid if all the things are valid
instance (Validity k, Validity v) => Validity (Map k v) where
    isValid m = all isValid (M.keys m) && all isValid m


