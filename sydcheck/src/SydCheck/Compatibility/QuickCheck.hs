{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module SydCheck.Compatibility.QuickCheck where

import SydCheck.Gen
import SydCheck.Property

type Property = forall ls. (TypedProperty ls)

type IsProperty a = forall ls. (IsTypedProperty ls a)

-- | A synonym for 'forAll' that ignores the shrinker.
{-# DEPRECATED forAllShrink "This function ignores the shrinker, you can use forAll instead." #-}
forAllShrink :: IsTypedProperty ls prop => Gen a -> (a -> [a]) -> (a -> prop) -> TypedProperty (a ': ls)
forAllShrink gen _ = forAll gen
