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
    validate (Object o) = o <?!> "Object"
    validate (Array a) = a <?!> "Array"
    validate (String t) = t <?!> "String"
    validate (Number s) = s <?!> "Number"
    validate (Bool b) = b <?!> "Bool"
    validate Null = mempty
