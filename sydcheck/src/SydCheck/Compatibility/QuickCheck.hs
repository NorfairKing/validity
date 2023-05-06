{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module SydCheck.Compatibility.QuickCheck where

import SydCheck.Gen
import SydCheck.Property

type Property = forall ls. (TypedPropertyT ls IO)

type IsProperty a = forall ls. (IsTypedPropertyT ls IO a)

-- | A synonym for 'forAll' that ignores the shrinker.
{-# DEPRECATED forAllShrink "This function ignores the shrinker, you can use forAll instead." #-}
forAllShrink :: IsTypedPropertyT ls m prop => Gen a -> (a -> [a]) -> (a -> prop) -> TypedPropertyT (a ': ls) m
forAllShrink gen _ = forAll gen
