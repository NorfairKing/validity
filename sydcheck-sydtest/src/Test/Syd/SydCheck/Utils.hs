{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Syd.SydCheck.Utils
  ( nameOf,
    genDescr,
    binRelStr,
  )
where

import Data.Typeable

nameOf ::
  forall a.
  Typeable a =>
  String
nameOf = showsPrec 10 (typeRep (Proxy @a)) ""

genDescr ::
  forall a.
  Typeable a =>
  String ->
  String
genDescr genname = unwords ["\"" ++ genname, "::", nameOf @a ++ "\""]

binRelStr ::
  forall a.
  Typeable a =>
  String ->
  String
binRelStr op = unwords ["(" ++ op ++ ")", "::", name, "->", name, "->", "Bool"]
  where
    name = nameOf @a
