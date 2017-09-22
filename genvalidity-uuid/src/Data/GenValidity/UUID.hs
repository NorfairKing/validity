{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.UUID where
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>))
import Data.Functor ((<$>))
#endif
import Data.GenValidity
import Data.UUID
import Data.Validity.UUID ()

instance GenUnchecked UUID where
    genUnchecked =
        fromWords <$> genUnchecked <*> genUnchecked <*> genUnchecked <*>
        genUnchecked

instance GenValid UUID
