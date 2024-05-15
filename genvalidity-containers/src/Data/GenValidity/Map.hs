{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Map
  ( genMapOf,
    shrinkMapOf,
  )
where

import Data.GenValidity
import Data.Map (Map)
import qualified Data.Map as M
import Data.Validity.Map ()
import Test.QuickCheck

instance (Show k, Ord k, GenValid k, GenValid v) => GenValid (Map k v) where
  genValid = genMapOf genValid
  shrinkValid = shrinkMapOf shrinkValid

genMapOf :: (Ord k) => Gen (k, v) -> Gen (Map k v)
genMapOf g = M.fromList <$> genListOf g

shrinkMapOf :: (Ord k) => ((k, v) -> [(k, v)]) -> Map k v -> [Map k v]
shrinkMapOf shrinker = fmap M.fromList . shrinkList shrinker . M.toList
