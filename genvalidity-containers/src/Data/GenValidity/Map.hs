module Data.GenValidity.Map where

import           Data.GenValidity
import           Data.Validity.Map ()

import           Data.Map         (Map)
import qualified Data.Map         as M

instance (Ord k, GenValidity k, GenValidity v) => GenValidity (Map k v) where
    genUnchecked = M.fromList <$> genUnchecked

    genValid     = M.fromList <$> genValid

    genInvalid   = M.fromList <$> genInvalid

