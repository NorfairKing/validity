{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.Aeson where
#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$>))
#endif
import Data.Foldable (toList)
import Data.GenValidity
import Data.GenValidity.HashMap ()
import Data.GenValidity.Scientific ()
import Data.GenValidity.Text ()
import Data.GenValidity.Vector ()
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
    shrinkUnchecked (Object hm) =
        (Object <$> shrinkUnchecked hm) ++
        toList hm ++ concatMap shrinkUnchecked (toList hm)
    shrinkUnchecked (Array a) =
        (Array <$> shrinkUnchecked a) ++
        toList a ++ concatMap shrinkUnchecked (toList a)
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
