{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Utilities for defining your own validity 'Spec's
--
-- You will need @TypeApplications@ to use these.
module Test.Syd.Validity.Utils
  ( nameOf,
    genDescr,
    binRelStr,
    shouldBeValid,
    shouldBeInvalid,
    Anon (..),
  )
where

import Data.Data
import Test.Syd.Validity.Property.Utils

nameOf ::
  forall a.
  (Typeable a) =>
  String
nameOf =
  let s = show $ typeRep (Proxy @a)
   in if ' ' `elem` s
        then "(" ++ s ++ ")"
        else s

genDescr ::
  forall a.
  (Typeable a) =>
  String ->
  String
genDescr genname = unwords ["\"" ++ genname, "::", nameOf @a ++ "\""]

binRelStr ::
  forall a.
  (Typeable a) =>
  String ->
  String
binRelStr op = unwords ["(" ++ op ++ ")", "::", name, "->", name, "->", "Bool"]
  where
    name = nameOf @a

newtype Anon a
  = Anon a

instance Show (Anon a) where
  show _ = "Anonymous"

instance Functor Anon where
  fmap f (Anon a) = Anon (f a)
