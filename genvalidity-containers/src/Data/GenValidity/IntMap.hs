{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.IntMap
  ( genIntMapOf,
    shrinkIntMapOf,
  )
where

import Data.GenValidity
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.Validity.IntMap ()
import Test.QuickCheck

instance (GenValid v) => GenValid (IntMap v) where
  genValid = genIntMapOf genValid
  shrinkValid = shrinkIntMapOf shrinkValid

genIntMapOf :: Gen (Int, v) -> Gen (IntMap v)
genIntMapOf g = M.fromList <$> genListOf g

shrinkIntMapOf :: ((Int, v) -> [(Int, v)]) -> IntMap v -> [IntMap v]
shrinkIntMapOf shrinker = fmap M.fromList . shrinkList shrinker . M.toList
