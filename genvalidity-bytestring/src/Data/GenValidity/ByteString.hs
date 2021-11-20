{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.ByteString where

import qualified Data.ByteString as SB
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

-- |
--
-- > genValid = SB.pack <$> genValid
-- > shrinkValid = fmap SB.pack . shrinkValid . SB.unpack
instance GenValid SB.ByteString where
  genValid = genStrictByteStringBy genValid
  shrinkValid = fmap SB.pack . shrinkValid . SB.unpack

genStrictByteStringBy :: Gen Word8 -> Gen SB.ByteString
genStrictByteStringBy (MkGen word8Func) = do
  len <- genListLength
  MkGen $ \qcgen size ->
    let go :: QCGen -> Maybe (Word8, QCGen)
        go qcg =
          let (qc1, qc2) = Random.split qcg
           in Just (word8Func qc1 size, qc2)
     in fst $ SB.unfoldrN len go qcgen

instance GenValid LB.ByteString where
  genValid = genLazyByteStringByStrictByteString genValid
  shrinkValid = fmap LB.pack . shrinkValid . LB.unpack

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

instance GenValid Short.ShortByteString where
  genValid = Short.pack <$> genValid
  shrinkValid = fmap Short.pack . shrinkValid . Short.unpack
