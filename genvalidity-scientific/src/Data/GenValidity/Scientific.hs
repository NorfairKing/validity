{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.Scientific where
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>))
import Data.Functor ((<$>))
#endif
import Data.GenValidity
import Data.Scientific
import Data.Validity.Scientific ()

instance GenUnchecked Scientific where
    genUnchecked = scientific <$> genUnchecked <*> genUnchecked
    shrinkUnchecked s =
        [ scientific c e
        | (c, e) <- shrinkUnchecked (coefficient s, base10Exponent s)
        ]

instance GenValid Scientific
