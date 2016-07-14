{-# LANGUAGE MultiParamTypeClasses #-}
module Data.GenRelativeValidity
    ( module Data.RelativeValidity
    , module Data.GenRelativeValidity
    ) where

import           Data.RelativeValidity
import           Data.GenValidity

import           Test.QuickCheck

import           Control.Monad   (forM)

class (GenValidity a, RelativeValidity a b) => GenRelativeValidity a b where
    genUncheckedFor :: b -> Gen a
    genUncheckedFor _ = genUnchecked

    genValidFor :: b -> Gen a
    genValidFor b = genValid `suchThat` (`isValidFor` b)

    genInvalidFor :: b -> Gen a
    genInvalidFor b = genUncheckedFor b `suchThat` (not . (`isValidFor` b))
    {-# MINIMAL #-}

