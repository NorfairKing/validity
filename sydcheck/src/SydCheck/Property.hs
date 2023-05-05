{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module SydCheck.Property where

import SydCheck.Gen
import SydCheck.GenValid

data TypedProperty ls where
  PropBool :: Bool -> TypedProperty '[]
  PropGen :: Gen a -> (a -> TypedProperty ls) -> TypedProperty (a ': ls)

class IsTypedProperty ls a | a -> ls where
  toTypedProperty :: a -> TypedProperty ls

instance IsTypedProperty ls (TypedProperty ls) where
  toTypedProperty = id

instance IsTypedProperty '[] Bool where
  toTypedProperty = PropBool

instance (GenValid a, IsTypedProperty ls b) => IsTypedProperty (a ': ls) (a -> b) where
  toTypedProperty func = forAll genValid $ \a -> func a

forAll :: IsTypedProperty ls prop => Gen a -> (a -> prop) -> TypedProperty (a ': ls)
forAll gen func = PropGen gen $ \a -> toTypedProperty (func a)
