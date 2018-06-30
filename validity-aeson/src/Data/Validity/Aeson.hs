{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.Aeson where

import Data.Aeson
import Data.Validity
import Data.Validity.HashMap ()
import Data.Validity.Scientific ()
import Data.Validity.Text ()
import Data.Validity.Vector ()

-- | A 'Value' is valid if the recursive components are valid.
instance Validity Value where
    validate (Object o) = annotate o "Object"
    validate (Array a) = annotate a "Array"
    validate (String t) = annotate t "String"
    validate (Number s) = annotate s "Number"
    validate (Bool b) = annotate b "Bool"
    validate Null = valid
