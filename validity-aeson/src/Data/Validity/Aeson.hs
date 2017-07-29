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
    isValid (Object o) = isValid o
    isValid (Array a) = isValid a
    isValid (String t) = isValid t
    isValid (Number s) = isValid s
    isValid (Bool b) = isValid b
    isValid Null = True
