{-# OPTIONS_GHC -Wno-orphans #-}

module Data.GenValidity.Set where

import Data.GenValidity
import Data.Validity.Set ()

import Data.Set (Set)
import qualified Data.Set as S

instance (Ord v, GenUnchecked v) =>
         GenUnchecked (Set v) where
    genUnchecked = S.fromList <$> genUnchecked

instance (Ord v, GenValid v) =>
         GenValid (Set v) where
    genValid = S.fromList <$> genValid

instance (Ord v, GenInvalid v) =>
         GenInvalid (Set v) where
    genInvalid = S.fromList <$> genInvalid
