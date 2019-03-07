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
    shrinkValid (Object hm) =
        (Object <$> shrinkValid hm) ++
        toList hm ++ concatMap shrinkValid (toList hm)
    shrinkValid (Array a) =
        (Array <$> shrinkValid a) ++
        toList a ++ concatMap shrinkValid (toList a)
    shrinkValid (String s) = String <$> shrinkValid s
    shrinkValid (Number s) = Number <$> shrinkValid s
    shrinkValid (Bool s) = Bool <$> shrinkValid s
    shrinkValid Null = []
