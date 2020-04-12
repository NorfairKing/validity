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
    shrinkUnchecked u =
        [ fromWords w1 w2 w3 w4
        | (w1, w2, w3, w4) <- shrinkUnchecked $ toWords u
        ]

instance GenValid UUID where
    genValid =
        fromWords <$> genValid <*> genValid <*> genValid <*>
        genValid
    shrinkValid u =
        [ fromWords w1 w2 w3 w4
        | (w1, w2, w3, w4) <- shrinkValid $ toWords u
        ]
