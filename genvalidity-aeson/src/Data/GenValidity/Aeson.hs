{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Aeson where

import Data.Aeson
import Data.Foldable (toList)
import Data.GenValidity
import Data.GenValidity.HashMap ()
import Data.GenValidity.Scientific ()
import Data.GenValidity.Text ()
import Data.GenValidity.Vector ()
import Data.Validity.Aeson ()
import Test.QuickCheck

instance GenValid Value where
  genValid =
    oneof
      [ Object <$> genValid,
        Array <$> genValid,
        String <$> genValid,
        Number <$> genValid,
        Bool <$> genValid,
        pure Null
      ]
  shrinkValid (Object hm) =
    toList hm
      ++ (Object <$> shrinkValid hm)
  shrinkValid (Array a) =
    toList a
      ++ (Array <$> shrinkValid a)
  shrinkValid (String s) = String <$> shrinkValid s
  shrinkValid (Number s) = Number <$> shrinkValid s
  shrinkValid (Bool s) = Bool <$> shrinkValid s
  shrinkValid Null = []
