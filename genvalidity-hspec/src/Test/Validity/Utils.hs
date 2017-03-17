{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Utilities for defining your own validity 'Spec's
--
-- You will need @TypeApplications@ to use these.
module Test.Validity.Utils
    ( (<==>)
    , (===>)
    , nameOf
    , genDescr
    , binRelStr
    , Anon(..)
    ) where

import Data.Data

import Test.Validity.Property.Utils

nameOf
    :: forall a.
       Typeable a
    => String
nameOf = show $ typeRep (Proxy @a)

genDescr
    :: forall a.
       Typeable a
    => String -> String
genDescr genname = unwords ["\"" ++ genname, "::", nameOf @a ++ "\""]

binRelStr
    :: forall a.
       Typeable a
    => String -> String
binRelStr op = unwords ["(" ++ op ++ ")", "::", name, "->", name, "->", "Bool"]
  where
    name = nameOf @a

data Anon a =
    Anon a

instance Show (Anon a) where
    show _ = "Anonymous"

instance Functor Anon where
    fmap f (Anon a) = Anon (f a)
