{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.Aeson where
#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$>))
#endif
import Data.GenValidity
import Data.GenValidity.HashMap ()
import Data.GenValidity.Text ()
import Data.GenValidity.Vector ()
import Data.GenValidity.Scientific ()
import Data.Validity.Aeson ()

import Data.Aeson

import Test.QuickCheck

instance GenUnchecked Value where
    genUnchecked =
        oneof
            [ Object <$> genUnchecked
            , Array <$> genUnchecked
            , String <$> genUnchecked
            , Number <$> genUnchecked
            , Bool <$> genUnchecked
            , pure Null
            ]
    shrinkUnchecked (Object hm) = Object <$> shrinkUnchecked hm
    shrinkUnchecked (Array a) = Array <$> shrinkUnchecked a
    shrinkUnchecked (String s) = String <$> shrinkUnchecked s
    shrinkUnchecked (Number s) = Number <$> shrinkUnchecked s
    shrinkUnchecked (Bool s) = Bool <$> shrinkUnchecked s
    shrinkUnchecked Null = []

instance GenValid Value where
    genValid =
        oneof
            [ Object <$> genValid
            , Array <$> genValid
            , String <$> genValid
            , Number <$> genValid
            , Bool <$> genValid
            , pure Null
            ]

instance GenInvalid Value where
    genInvalid =
        oneof
            [Object <$> genInvalid, Array <$> genInvalid, String <$> genInvalid]
