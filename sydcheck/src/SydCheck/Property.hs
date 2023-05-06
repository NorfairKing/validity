{-# LANGUAGE ConstraintKinds #-}
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

import Data.Kind
import SydCheck.Gen
import SydCheck.GenValid

type TypedProperty ls = TypedPropertyT ls IO

type IsTypedProperty ls = IsTypedPropertyT ls IO

data TypedPropertyT (ls :: [Type]) (m :: Type -> Type) where
  PropBool :: Bool -> TypedPropertyT '[] m
  PropGen :: Gen a -> (a -> TypedPropertyT ls m) -> TypedPropertyT (a ': ls) m

class IsTypedPropertyT ls m a | a -> ls where
  toTypedPropertyT :: a -> TypedPropertyT ls m

instance m1 ~ m2 => IsTypedPropertyT ls m1 (TypedPropertyT ls m2) where
  toTypedPropertyT = id

instance IsTypedPropertyT '[] m Bool where
  toTypedPropertyT = PropBool

instance (GenValid a, IsTypedPropertyT ls m b) => IsTypedPropertyT (a ': ls) m (a -> b) where
  toTypedPropertyT func = forAll genValid $ \a -> func a

forAll ::
  IsTypedPropertyT ls m prop =>
  Gen a ->
  (a -> prop) ->
  TypedPropertyT (a ': ls) m
forAll gen func = PropGen gen $ \a -> toTypedPropertyT (func a)

forAllValid ::
  (GenValid a, IsTypedPropertyT ls m prop) =>
  (a -> prop) ->
  TypedPropertyT (a ': ls) m
forAllValid = forAll genValid
