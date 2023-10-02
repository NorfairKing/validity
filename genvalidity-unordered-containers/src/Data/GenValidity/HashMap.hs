{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.GenValidity.HashMap where

import Data.GenValidity
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.Validity.HashMap ()

instance (Hashable k, Eq k, GenValid k, GenValid v) => GenValid (HashMap k v) where
  genValid = HM.fromList <$> genValid
  shrinkValid = fmap HM.fromList . shrinkValid . HM.toList
