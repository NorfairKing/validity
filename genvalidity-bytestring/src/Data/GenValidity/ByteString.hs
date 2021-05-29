{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#if MIN_VERSION_base(4,9,0)
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
#endif
module Data.GenValidity.ByteString where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>), pure)
import Data.Functor ((<$>))
#endif
import qualified Data.ByteString as SB
import qualified Data.ByteString.Internal as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Internal as LB
import qualified Data.ByteString.Short as Short
import Data.GenValidity
import Data.Validity.ByteString ()
import Data.Word (Word8)
import System.Random as Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

#if MIN_VERSION_base(4,9,0)
import GHC.TypeLits
#endif

-- |
--
-- > genValid = SB.pack <$> genValid
-- > shrinkValid = fmap SB.pack . shrinkValid . SB.unpack
instance GenValid SB.ByteString where
  genValid = genStrictByteStringBy genValid
  shrinkValid = fmap SB.pack . shrinkValid . SB.unpack

#if MIN_VERSION_base(4,9,0)
-- If you see this error and want to learn more, have a look at docs/BYTESTRING.md
instance GHC.TypeLits.TypeError ('GHC.TypeLits.Text "The GenUnchecked Data.ByteString.ByteString is disabled:" 'GHC.TypeLits.:$$: 'GHC.TypeLits.Text "Do not instantiate GenUnchecked, instantiate GenValid instead") =>
         GenUnchecked SB.ByteString where
    genUnchecked = error "unreachable"
    shrinkUnchecked = error "unreachable"
#endif

genStrictByteStringBy :: Gen Word8 -> Gen SB.ByteString
genStrictByteStringBy (MkGen word8Func) = do
  len <- genListLength
  MkGen $ \qcgen size ->
    let go :: QCGen -> Maybe (Word8, QCGen)
        go qcg =
          let (qc1, qc2) = Random.split qcg
           in Just (word8Func qc1 size, qc2)
     in fst $ SB.unfoldrN len go qcgen

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
  genValid = genLazyByteStringBy genValid
  shrinkValid = fmap LB.pack . shrinkValid . LB.unpack

#if MIN_VERSION_base(4,9,0)
-- If you see this error and want to learn more, have a look at docs/BYTESTRING.md
instance GHC.TypeLits.TypeError ('GHC.TypeLits.Text "The GenUnchecked Data.ByteString.Lazy.ByteString is disabled:" 'GHC.TypeLits.:$$: 'GHC.TypeLits.Text "Do not instantiate GenUnchecked, instantiate GenValid instead") =>
         GenUnchecked LB.ByteString where
    genUnchecked = error "unreachable"
    shrinkUnchecked = error "unreachable"
#endif

genLazyByteStringBy :: Gen Word8 -> Gen LB.ByteString
genLazyByteStringBy gen = genLazyByteStringByStrictByteString (genStrictByteStringBy gen)

genLazyByteStringByStrictByteString :: Gen SB.ByteString -> Gen LB.ByteString
genLazyByteStringByStrictByteString gen =
  sized $ \s -> do
    ss <- arbPartition s
    go ss
  where
    go [] = pure LB.Empty
    go (s : ss) = LB.Chunk <$> resize s gen <*> go ss

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
