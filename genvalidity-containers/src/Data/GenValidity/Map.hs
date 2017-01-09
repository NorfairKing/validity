module Data.GenValidity.Map where

import Data.GenValidity
import Data.Validity.Map ()

import Data.Map (Map)
import qualified Data.Map as M

instance (Ord k, GenUnchecked k, GenUnchecked v) =>
         GenUnchecked (Map k v) where
    genUnchecked = M.fromList <$> genUnchecked

instance (Ord k, GenValid k, GenValid v) =>
         GenValid (Map k v) where
    genValid = M.fromList <$> genValid

instance (Ord k, GenInvalid k, GenInvalid v) =>
         GenInvalid (Map k v) where
    genInvalid = M.fromList <$> genInvalid
