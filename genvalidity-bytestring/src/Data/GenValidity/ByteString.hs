{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.ByteString where

import Data.GenValidity
import Data.Validity.ByteString ()
import Test.QuickCheck
#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$>))
#endif
import qualified Data.ByteString as SB
import qualified Data.ByteString.Internal as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Internal as LB

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

instance GenUnchecked LB.ByteString where
    genUnchecked =
        sized $ \n ->
            case n of
                0 -> pure LB.Empty
                n -> do
                    (a, b) <- genSplit n
                    sb <- resize a genUnchecked
                    lb <- resize b genUnchecked
                    pure $ LB.Chunk sb lb
    shrinkUnchecked LB.Empty = []
    shrinkUnchecked (LB.Chunk sb lb) =
        LB.Empty : [LB.Chunk sb' lb' | (sb', lb') <- shrinkUnchecked (sb, lb)]

instance GenValid LB.ByteString where
    genValid = LB.pack <$> genValid
    shrinkValid = fmap LB.pack . shrinkValid . LB.unpack

instance GenInvalid LB.ByteString
