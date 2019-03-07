{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.ByteString where

import Data.GenValidity
import Data.Validity.ByteString ()
import Test.QuickCheck
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>), pure)
import Data.Functor ((<$>))
#endif
import qualified Data.ByteString as SB
import qualified Data.ByteString.Internal as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Internal as LB
import qualified Data.ByteString.Short as Short

-- |
--
-- > genValid = SB.pack <$> genValid
-- > shrinkValid = fmap SB.pack . shrinkValid . SB.unpack
instance GenValid SB.ByteString where
    genValid = SB.pack <$> genValid
    shrinkValid = fmap SB.pack . shrinkValid . SB.unpack

-- | WARNING: Unchecked ByteStrings are __seriously__ broken.
--
-- The pointer may still point to something which is fine, but
-- the offset and length will most likely be complete nonsense.
-- This will most-likely lead to segfaults.
--
-- This means that 'genUnchecked' will generate seriously broken 'ByteString' values.
-- This is __intended__. If you need valid 'ByteString' values, use 'GenValid' instead.
--
-- Make sure to not use any test suite combinators or property combinators that involve
-- 'GenInvalid' (like 'genValiditySpec') on types that contain 'ByteString' values.
genTrulyUncheckedStrictByteString :: Gen SB.ByteString
genTrulyUncheckedStrictByteString = do
    ws <- genUnchecked
    -- TODO what do we do about the foreign pointer?
    let SB.PS p _ _ = SB.pack ws
    SB.PS p <$> genUnchecked <*> genUnchecked

shrinkTrulyUncheckedStrictByteString :: SB.ByteString -> [SB.ByteString]
shrinkTrulyUncheckedStrictByteString (SB.PS p o l) =
    [SB.PS p o' l' | (o', l') <- shrinkUnchecked (o, l)]

instance GenValid LB.ByteString where
    genValid = LB.pack <$> genValid
    shrinkValid = fmap LB.pack . shrinkValid . LB.unpack

-- | WARNING: Unchecked ByteStrings are __seriously__ broken.
--
-- See 'genTrulyUncheckedStrictByteString'
genTrulyUncheckedLazyByteString :: Gen LB.ByteString
genTrulyUncheckedLazyByteString =
    sized $ \n ->
        case n of
            0 -> pure LB.Empty
            _ -> do
                (a, b) <- genSplit n
                sb <- resize a genTrulyUncheckedStrictByteString
                lb <- resize b genTrulyUncheckedLazyByteString
                pure $ LB.Chunk sb lb

shrinkTrulyUncheckedLazyByteString :: LB.ByteString -> [LB.ByteString]
shrinkTrulyUncheckedLazyByteString lb_ =
    case lb_ of
        LB.Empty -> []
        (LB.Chunk sb lb) ->
            LB.Empty :
            [ LB.Chunk sb' lb'
            | (sb', lb') <-
                  shrinkTuple
                      shrinkTrulyUncheckedStrictByteString
                      shrinkTrulyUncheckedLazyByteString
                      (sb, lb)
            ]


instance GenUnchecked Short.ShortByteString where
    genUnchecked = Short.pack <$> genValid
    shrinkUnchecked = fmap Short.pack . shrinkUnchecked . Short.unpack

instance GenValid Short.ShortByteString
