{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
module Test.Validity.Utils
    ( (<==>)
    , (===>)
    , nameOf
    , binRelStr
    ) where

import           Data.Data

(===>) :: Bool -> Bool -> Bool
(===>) a b = not a || b

(<==>) :: Bool -> Bool -> Bool
(<==>) a b = a ===> b && b ===> a

nameOf :: forall a. Typeable a => String
nameOf =
    let (_, [ty]) = splitTyConApp $ typeOf (Proxy @a)
    in show ty

binRelStr :: forall a. Typeable a => String -> String
binRelStr op = unwords
    [ "(" ++ op ++ ")"
    , "::"
    , name
    , "->"
    , name
    , "->"
    , "Bool"
    ]
  where name = nameOf @a

