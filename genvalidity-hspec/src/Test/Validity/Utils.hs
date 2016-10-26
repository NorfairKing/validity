{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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

nameOf :: Typeable a => Proxy a -> String
nameOf proxy =
    let (_, [ty]) = splitTyConApp $ typeOf proxy
    in show ty

binRelStr :: Typeable a => String -> Proxy a -> String
binRelStr op proxy = unwords
    [ "(" ++ op ++ ")"
    , "::"
    , name
    , "->"
    , name
    , "->"
    , "Bool"
    ]
  where name = nameOf proxy

