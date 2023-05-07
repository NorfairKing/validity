{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module SydCheck.Property where

import Control.Exception
import Control.Monad
import Data.Kind
import SydCheck.Gen
import SydCheck.GenValid

type TypedProperty ls = TypedPropertyT ls IO ()

type IsTypedProperty ls = IsTypedPropertyT ls IO

data TypedPropertyT (ls :: [Type]) (m :: Type -> Type) a where
  PropAction :: m r -> TypedPropertyT '[] m r
  PropGen ::
    Show a =>
    Gen a ->
    (a -> TypedPropertyT ls m r) ->
    TypedPropertyT (a ': ls) m r
  PropFmap ::
    (a -> b) ->
    TypedPropertyT ls m a ->
    TypedPropertyT ls m b
  PropPure :: a -> TypedPropertyT ls m a
  PropAp ::
    TypedPropertyT ls m (a -> b) ->
    TypedPropertyT ls m a ->
    TypedPropertyT ls m b
  PropBind ::
    TypedPropertyT ls m a ->
    (a -> TypedPropertyT ls m b) ->
    TypedPropertyT ls m b

instance Functor (TypedPropertyT ls m) where
  fmap = PropFmap

instance Applicative (TypedPropertyT ls m) where
  pure = PropPure
  (<*>) = PropAp

instance Monad (TypedPropertyT ls m) where
  (>>=) = PropBind

class IsTypedPropertyT ls m a where
  toTypedPropertyT :: a -> TypedPropertyT ls m ()

instance IsTypedPropertyT ls m (TypedPropertyT ls m ()) where
  toTypedPropertyT = id

instance IsTypedPropertyT '[] IO Bool where
  toTypedPropertyT b =
    PropAction $
      when (not b) $
        throwIO $
          userError "Bool was false."

instance IsTypedPropertyT '[] m (m ()) where
  toTypedPropertyT = PropAction

instance (Show a, GenValid a, IsTypedPropertyT ls m b) => IsTypedPropertyT (a ': ls) m (a -> b) where
  toTypedPropertyT func = forAll genValid $ \a -> func a

forAll ::
  (Show a, IsTypedPropertyT ls m prop) =>
  Gen a ->
  (a -> prop) ->
  TypedPropertyT (a ': ls) m ()
forAll gen func = PropGen gen $ \a -> toTypedPropertyT (func a)

forAllValid ::
  (Show a, GenValid a, IsTypedPropertyT ls m prop) =>
  (a -> prop) ->
  TypedPropertyT (a ': ls) m ()
forAllValid = forAll genValid
