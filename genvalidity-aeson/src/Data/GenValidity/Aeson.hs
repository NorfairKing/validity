{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Aeson where

import Data.Aeson
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
#endif
import Data.Foldable (toList)
import Data.GenValidity
import Data.GenValidity.HashMap ()
import Data.GenValidity.Scientific ()
import Data.GenValidity.Text ()
import Data.GenValidity.Vector ()
import Data.Validity.Aeson ()
import Test.QuickCheck

#if MIN_VERSION_aeson(2,0,0)
instance GenValid Key where
   genValid = K.fromText <$> genValid
   shrinkValid = fmap K.fromText . shrinkValid . K.toText

instance (GenValid v) => GenValid (KeyMap v) where
  genValid = KM.fromList <$> genValid
  shrinkValid = fmap KM.fromList . shrinkValid . KM.toList
#endif

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
    Null :
    (toList hm ++ (Object <$> shrinkValid hm))
  shrinkValid (Array a) =
    Null :
    (toList a ++ (Array <$> shrinkValid a))
  shrinkValid (String s) = Null : (String <$> shrinkValid s)
  shrinkValid (Number s) = Null : (Number <$> shrinkValid s)
  shrinkValid (Bool s) = Null : (Bool <$> shrinkValid s)
  shrinkValid Null = []
