{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.ByteString where

import Data.GenValidity
import Data.Validity.ByteString ()
#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$>))
#endif
import qualified Data.ByteString as SB
import qualified Data.ByteString.Internal as SB

-- TODO what do we do about the foreign pointer?
instance GenUnchecked SB.ByteString where
    genUnchecked = do
        ws <- genUnchecked
        let SB.PS p o l = SB.pack ws
        SB.PS p <$> genUnchecked <*> genUnchecked
    shrinkUnchecked (SB.PS p o l) =
        [SB.PS p o' l' | (o', l') <- shrinkUnchecked (o, l)]

instance GenValid SB.ByteString where
    genValid = SB.pack <$> genValid
    shrinkValid = fmap SB.pack . shrinkValid . SB.unpack

instance GenInvalid SB.ByteString
