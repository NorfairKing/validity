{-# LANGUAGE MultiParamTypeClasses #-}

module Data.GenRelativeValidity
  ( module Data.RelativeValidity,
    module Data.GenRelativeValidity,
  )
where

import Data.GenValidity
import Data.RelativeValidity
import Test.QuickCheck

class
  (GenUnchecked a, RelativeValidity a b) =>
  GenRelativeUnchecked a b
  where
  genUncheckedFor :: b -> Gen a
  genUncheckedFor _ = genUnchecked

class
  (GenValid a, RelativeValidity a b) =>
  GenRelativeValid a b
  where
  genValidFor :: b -> Gen a
  genValidFor b = genValid `suchThat` (`isValidFor` b)

class
  (GenUnchecked a, RelativeValidity a b, GenRelativeUnchecked a b) =>
  GenRelativeInvalid a b
  where
  genInvalidFor :: b -> Gen a
  genInvalidFor b = genUncheckedFor b `suchThat` (not . (`isValidFor` b))
