{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Validity.Utils
    ( (<==>)
    , (===>)
    , nameOf
    , genDescr
    , binRelStr
    ) where

import Data.Data

(===>) :: Bool -> Bool -> Bool
(===>) a b = not a || b

(<==>) :: Bool -> Bool -> Bool
(<==>) a b = a ===> b && b ===> a

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
