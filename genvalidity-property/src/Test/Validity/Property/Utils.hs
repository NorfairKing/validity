module Test.Validity.Property.Utils
    ( (<==>)
    , (===>)
    ) where

(===>) :: Bool -> Bool -> Bool
(===>) a b = not a || b

(<==>) :: Bool -> Bool -> Bool
(<==>) a b = a ===> b && b ===> a
